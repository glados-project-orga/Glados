fun main() : void {
    try {
        throw "An error occurred";
    } catch (e) {
        print(e);
    }
}