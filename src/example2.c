void main() {
  int t1, t2, t3;
  
  t1 = g(0); // c1
  t2 = g(243); // c2
  t3 = g(255); // c3
}

int g(int v) {
  return f(v);
}

int f(int v) {
  return f(v+1);
}
