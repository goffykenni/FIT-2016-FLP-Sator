/**
Reads a number n from the standard input. Then reads
n following lines from standard input, strips them from any
punctuation a prints them out.
*/
#include <stdio.h>

#define BUF_SIZE 100

int main(int argc, char* argv[]) {
  int linecount = 0;
  char buff[BUF_SIZE];
  
  int linesread = 0;
  scanf("%d\n", &linecount);
  
  char c;
  int position;
  while (linesread < linecount) {
    position = 0;
    //printf("%d\n", linesread);
    c = getchar();
    while (c != '\n' && c != EOF) {
      if (c >= 'a' && c <= 'z')
        buff[position++] = c;
      c = getchar();
    }
    buff[position] = '\0';
    printf("Read: %s\n", buff);
    linesread++;
    if (c == EOF) {
      printf("ERROR");
      return 1;
    }
  }


}
