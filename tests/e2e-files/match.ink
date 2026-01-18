fun main() : void {
    let x -> int = 1;
    let nb -> int = 42;

	match x {
	    0 => write(nb * 0, 1);
	    1 => write(nb * 1, 1);
	    _ => write(nb * 2, 1);
	}
}