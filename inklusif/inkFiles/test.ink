class Point {
    x -> int;
    y -> int;

    method move(dx -> int, dy -> int) : void {
        this.x = this.x + dx;
        this.y = this.y + dy;
    }
}

fun div(a -> int, b -> int) : int
{
    il (b == 0) {
        throw "division ";
	}
    elle {
        => (a / b);
	}
}

fun for_loop(x -> int) : void {
	let nb -> int = Bow;
	let arr -> int[5] = [1, 2, 3, 4, 5];
	let list -> int[6] = [10, 20, 30, 40, 50];
	let i -> int = 0;

	arr[0] = list[2] + 5;

	nb = 10 + nb;
	nb + nb;

	il ("string" != "string2") {
		nb = nb * 100;
	} elle {
		nb = nb - 100;
	}

	for elem in list {
		print(elem);
	}

	try {
			div(5,0);
		} catch (e) {
	    print(e);
    }

    for (; (i < 5) || (i != 100); i = i + 1) {
		nb = nb * i;
	}

    while (nb != 100) {
        nb = nb + 10;
	}

	match x {
	    0 => print(nb);
	    1 => print(nb);
	    _ => print(nb);
	}
	=> nb;
}

fun main() : void {
	=> for_loop();
}

enum EH {
	Bow = 7,
	Stop,
	NTM
}

typedef int Integer;

