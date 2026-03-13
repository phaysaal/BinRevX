void array_fill_ptr(int *a, int n, int v) {
  int *p = a;
  int *end = a + n;
  while (p < end) {
    *p = v;
    p++;
  }
}
