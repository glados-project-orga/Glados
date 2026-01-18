class Point {
    x -> int;
    arr -> int[1];
    p -> Point;
    method move(dx -> int, dy -> int) : void {
        this.x = this.arr[0] + dx + dy;
    }
}

fun main() : void {
    let pt -> Point = new Point();
    pt.arr = [1];
    pt.p = new Point();
    pt.p.x = 0;
    pt.p.arr = [2];
    pt.p.move(1, 1);
    write(pt.p.x, 1);
}
