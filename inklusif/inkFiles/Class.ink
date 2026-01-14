class Point {
    x -> int;
    y -> int;

    method move(dx -> int, dy -> int) : void {
        this.x = this.x + dx;
        this.y = this.y + dy;
    }
}

fun main() : void {
    let p -> Point = new Point();

    p.x = 10;
    p.y = 20;
    p.move(5, -10);

    print("Point coordinates: (",  p.x, ", ",  p.y, ")");
}