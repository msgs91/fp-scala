package ds;

import java.util.Vector;

//points to think abt
//1) Integer only collection
//2) O(1) for all operations => cant make tradeoffs - only mathematical / binary CPU bound operations considered O(1)
public class IntegerCollection {

    Vector<Integer> vect = new Vector<>();
    private int add = 0;
    private int mult = 1;

    public int get(int index) {
        return vect.get(index) * mult + add;
    }

    public void append(int entry) {
        vect.add(entry);
    }

    public void addToAll(int number){
        this.add += number;
    }

    public void multiplyAll(int number){
        this.mult *= number;
        this.add *= number;
    }


}
