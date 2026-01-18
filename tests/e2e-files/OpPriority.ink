fun main() : void {
    let a -> int = 10;
    let b -> int = 20;
    let c -> int = 30;

    let result1 -> int = a + b * c;      
    let result2 -> int = (a + b) * c;    

    write(result1, 1);
    write(result2, 1);
}
