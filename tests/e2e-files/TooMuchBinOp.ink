fun main() : void {
    let op -> int = 0;

    op += 10;
    op *= 2;
    op -= op;
    op += 100;
    op /= 2;
    op + op;
    op - op;
    op / op;
    op * op;
    op = op;

    write(op, 1);
}
