int string_strlen_loop(char *s) {
  int n = 0;
  while (s[n] != 0) {
    n++;
  }
  return n;
}
