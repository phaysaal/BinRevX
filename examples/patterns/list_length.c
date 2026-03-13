struct Node {
  int value;
  struct Node *next;
};

int list_length(struct Node *head) {
  int n = 0;
  while (head != 0) {
    n++;
    head = head->next;
  }
  return n;
}
