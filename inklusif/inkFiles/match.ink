fun main() : void {
    let x -> int = 1;
    let nb -> int = 42;

	match x {
	    0 => print(nb * 0);
	    1 => print(nb * 1);
	    _ => print(nb * 2);
	}
}