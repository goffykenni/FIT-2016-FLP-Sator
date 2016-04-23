#include <stdio.h>

int bin_search(const int *arr, int arr_size, int key) {
  int l = 0, r = arr_size - 1;
  int m;
  while (r >= l) {
    m = (l + r) / 2;
    if (arr[m] > key)
      r = m - 1; /* Move to the left part */
    else if (arr[m] < key)
      l = m + 1; /* Move to the right part */
    else {
      return m; /* Match */
    }
  }
  return -1;
}

int main(int argc, char *argv[]) {
  int arr[] = {1,2,4,6,6,8,13,14,22};
  printf("%d\n", bin_search(arr, 9, 14));
  return 0;
}