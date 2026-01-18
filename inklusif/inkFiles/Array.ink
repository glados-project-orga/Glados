fun main() : void {
    let arr -> int[5] = [1, 2, 3, 4, 5];

    for (let i -> int = 0; i < 5; i = i + 1) {
        print(arr[i]);
    }

    arr[2] = 10;


    for (let i -> int = 0; i < 5; i = i + 1) {
        print(arr[i]);
    }
}