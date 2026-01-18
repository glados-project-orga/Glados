fun main() : void {
    let arr -> int[5] = [1, 2, 3, 4, 5];

    for (let i -> int = 0; i < 5; i = i + 1) {
        write(arr[i], 1);
    }

    arr[2] = 10;

    for (let y -> int = 0; y < 5; y = y + 1) {
        write(arr[y], 1);
    }
}
