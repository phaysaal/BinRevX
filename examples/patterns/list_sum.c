struct Node {
  int value;
  struct Node *next;
};

int list_sum(struct Node *head) {
  int sum = 0;
  while (head != 0) {
    sum += head->value;
    head = head->next;
  }
  return sum;
}
