int array_sum_idx(int *a, int n) {
  int i = 0;
  int sum = 0;
  while (i < n) {
    sum += a[i];
    i++;
  }
  return sum;
}
