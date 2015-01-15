//// simple test for embedding compiled code


#include <stdio.h>
#include <assert.h>
#include "pc.h"


extern X prolog(int argc, char *argv[], X result, int *exit_code);


int main()
{
  char *argv[] = { "" };
  X one = prolog(1, argv, NULL, NULL);
  assert(one == word_to_fixnum(1));
  int x;
  X two = prolog(0, NULL, word_to_fixnum(123), &x);
  assert(x == EXIT_SUCCESS);
  assert(two == word_to_fixnum(2));
  X three = prolog(0, NULL, word_to_fixnum(456), &x);
  assert(x == EXIT_SUCCESS);
}
