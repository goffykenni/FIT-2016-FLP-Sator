/**
Reads a number n from the standard input. Then reads
n following lines from standard input, strips them from any
punctuation a prints them out.
*/
#include <stdio.h>

/* 10000 according to spec. + 2 for terminator and new-line character,
which is saved by fgets into the buffer. */
#define BUF_SIZE 10002

/* Strips the given zero-terminated string from all characters
but non-capital leters (a-z). */
int strip(char *buff) {
  /* Process the string from the left to the right and copy all its
   * characters to the left by the amount of skipped (non a-z) characters.*/ 
  int position = 0, offset = 0;
  while (buff[position] != '\0') {
    if (buff[position] >= 'a' && buff[position] <= 'z')
      buff[position - offset] = buff[position];
    else
      offset++;
    position++;
  }
  /* Add zero terminator */
  buff[position - offset] = '\0';
  return position - offset;
}

int integer_sqrt(int n) {
  int exp = 1; int val = 1;
  if (n < 1)
    return 0;
  while (val < n) {
    ++exp;
    val = exp * exp;
  }
  
  return (val == n) ? exp : 0;
}

/* Checks, whether there exists a k > 0 such that the
 * zero terminated string inside the given buffer is a k-palindrome
 * and returns such k, if it exists and 0 otherwise. */
int get_grid_size(const char *buff, int len) {
  /* First check, if it can fit some square at all */
  int size = integer_sqrt(len);
  /* If size > 0, check wheter it is size-palindrome */
  if (size) {    
    int row, col, lin_pos, tr_lin_pos;
    char c;
    for (row = 0; row < size; row++) {
      for (col = row; col < size; col++) {
        /* Linearize the position inside the grid */
        lin_pos = col + row * size;
        /* Get position with swapped row and col (transposition)*/
        tr_lin_pos = row + col * size;
        /* Check palindromic property */
        c = buff[lin_pos];
        if (buff[len - lin_pos - 1] != c ||
            buff[tr_lin_pos] != c ||
            buff[len - tr_lin_pos - 1] != c)
          return 0;          
      }
    }
  }  
  return size;
}

int main(int argc, char* argv[]) {
  int linecount = 0;
  char buff[BUF_SIZE];
  int i, len, result;
  
  scanf("%d ", &linecount);  
  printf("Lines to proess: %d\n", linecount);
  for (i = 1; i <= linecount; i++) {
    /* Try reading from standard input */
    if (fgets(buff, BUF_SIZE, stdin) != NULL) {
      /* Filter input here */
      len = strip(buff);
      /* Process here */
      result = get_grid_size(buff, len);
      /* Write output here */
      /*printf("Test %d: (len=%d): %s >> %d\n", i, len, "N/A", result);*/
      printf("Test %d: %d\n", i, result);
    }
  }
  return 0;
}
