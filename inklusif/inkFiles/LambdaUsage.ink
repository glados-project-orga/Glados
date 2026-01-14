fun main() : void {
    let multiply -> (int, int) => int = [x, y] {
            => x * y;
        };

    print(multiply(3, 4));
}
