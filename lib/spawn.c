/* library for spawning subprocesses (C part) */


#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>


static int handling_sigchld = 0;
static volatile int sigchld_occurred = 0;


static void sigchld_handler(int sig) 
{
  sigchld_occurred = 1;
  signal(sig, sigchld_handler);
}


PRIMITIVE(spawn, X cmd, X args, X rpid, X rfdin, X rfdout)
{
  int fds1[ 2 ], fds2[ 2 ];
  pid_t pid;
  static XCHAR *argbuf[ 100 ];
  int argc = 1;
  int len;

  /* setup SIGCHLD handler */
  if(!handling_sigchld) {
    signal(SIGCHLD, sigchld_handler);
    handling_sigchld = 1;
  }

  /* copy command + arguments */
  argbuf[ 0 ] = strdup(to_string(cmd, &len));

  while(args != END_OF_LIST_VAL) {
    argbuf[ argc++ ] = strdup(to_string(slot_ref(args, 0), &len));
    args = slot_ref(args, 1);
  }

  argbuf[ argc ] = NULL;

  /* create pipes */
  if(pipe(fds1) == -1)
    system_error(strerror(errno));

  if(pipe(fds2) == -1)
    system_error(strerror(errno));

  pid = fork();

  if(pid == -1)
    system_error(strerror(errno));

  if(pid == 0) {		/* child */
    /* redirect std-I/O */
    if(dup2(fds1[ 0 ], STDIN_FILENO) == -1)
      system_error(strerror(errno));

    if(dup2(fds2[ 1 ], STDOUT_FILENO) == -1)
      system_error(strerror(errno));

    /* run program */
    execvp(argbuf[ 0 ], argbuf);
    system_error(strerror(errno));
  }
  
  /* parent */

  for(int i = 0; i < argc; ++i)
    free(argbuf[ i ]);

  return unify(rpid, word_to_fixnum(pid)) &&
    unify(rfdin, word_to_fixnum(fds2[ 0 ])) &&
    unify(rfdout, word_to_fixnum(fds1[ 1 ]));
}


/*
PRIMITIVE(open_fd, X fd, X input, X mode, X data, X result) 
{
  int len;
  FILE *fp = fdopen(fixnum_to_word(fd), to_string(mode, &len));
  X port = PORT(fp, input, ONE, data);
  return unify(port, result);
}
*/


PRIMITIVE(reap_children, X pid)
{
  if(sigchld_occurred) {
    /* race */
    sigchld_occurred = 0;
    int status;
    pid_t pw = wait(&status);

    if(pw == -1)
      system_error(strerror(errno));

    return unify(pid, word_to_fixnum(pw));
  }

  return 0;
}


PRIMITIVE(close_fd, X fd) 
{
  if(close(fixnum_to_word(fd)) == -1)
    system_error(strerror(errno));

  return 1;
}


PRIMITIVE(raw_read, X fd, X count, X bytes)
{
  int fno = fixnum_to_word(check_fixnum(fd));
  XWORD n = fixnum_to_word(check_fixnum(count));
  ensure_string_buffer(n);
  size_t total = read(fno, string_buffer, n);

  if(total == -1)
    system_error(strerror(errno));

  return unify(bytes, string_to_list(string_buffer, total));
}


PRIMITIVE(raw_write, X fd, X bytes, X written)
{
  int fno = fixnum_to_word(check_fixnum(fd));
  int len;
  XCHAR *ptr = to_string(bytes, &len);
  size_t total = write(fno, string_buffer, len);

  if(total == -1)
    system_error(strerror(errno));

  return unify(written, word_to_fixnum(total));
}
