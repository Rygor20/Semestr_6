import java.util.Objects;
import java.util.Scanner;

public class FiniteField {
    private static final long characteristic = 1234567891;

    private long value;

    private long toRange(long x) {
        x %= characteristic;
            
        while (x < 0) {
            x += characteristic;
        }

        return x;
    }

    private long invert(long x) {
        for(long i = 0; i < characteristic; i++) {
            if((long)x*i % characteristic == 1) {
                return i;
            }
        }
        return -1;
    }

    public FiniteField() {
        this.value = 0;
    }

    public FiniteField(long val) {
        this.value = toRange(val);
    }

    public static long getCharacteristic() {
        return characteristic;
    }

    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof FiniteField)) return false;
        FiniteField other = (FiniteField) obj;
        return value == other.value;
    }

    public boolean notEquals(FiniteField other) {
        return !this.equals(other);
    }

    public boolean lessThanOrEqual(FiniteField other) {
        return value <= other.value;
    }

    public boolean greaterThanOrEqual(FiniteField other) {
        return value >= other.value;
    }

    public boolean lessThan(FiniteField other) {
        return value < other.value;
    }

    public boolean greaterThan(FiniteField other) {
        return value > other.value;
    }

    public FiniteField add(FiniteField other) {
        return new FiniteField(value + other.value);
    }

    public FiniteField subtract(FiniteField other) {
        return new FiniteField(value - other.value);
    }

    public FiniteField multiply(FiniteField other) {
        return new FiniteField(value * other.value);
    }

    public FiniteField divide(FiniteField other) {
        if(other.value == 0) {
                throw new ArithmeticException("Error: Dividing by zero");
            }
            
            long inv = invert(other.value);
            if(inv == -1) {
                throw new ArithmeticException("Error: Irreversible element");
            }
        long result = (long) this.value * inv % characteristic;
        return new FiniteField((long) (result < 0 ? result + characteristic : result));
    }

    public void addAssign(FiniteField other) {
        value = toRange(value + other.value);
    }

    public void subtractAssign(FiniteField other) {
        value = toRange(value - other.value);
    }

    public void multiplyAssign(FiniteField other) {
        value = toRange(value * other.value);
    }

    public void divideAssign(FiniteField other) {
        value = toRange((this.divide(other)).value);
        // this.value = this.divide(other).value;
    }

    public long getValue(){
        return value;
    }

    @Override
    public String toString() {
        return Long.toString(value);
    }

    public static void main(String[] args) {
        FiniteField a = new FiniteField(5);
        FiniteField b = new FiniteField(1234576);
        FiniteField c;
        System.out.println("Given 'a': " + a + " Given 'b': " + b);
        System.out.println("Characteristic: " + FiniteField.getCharacteristic());
        
        System.out.println("a == b ? " + a.equals(b));
        System.out.println("a != b ? " + a.notEquals(b));
        System.out.println("a <= b ? " + a.lessThanOrEqual(b));
        System.out.println("a >= b ? " + a.greaterThanOrEqual(b));
        System.out.println("a < b ? " + a.lessThan(b));
        System.out.println("a > b ? " + a.greaterThan(b));

        c = a.add(b);
        System.out.println("a + b = " + c);

        c = a.subtract(b);
        System.out.println("a - b = " + c);

        c = a.multiply(b);
        System.out.println("a * b = " + c);

        c = a.divide(b);
        System.out.println("a / b = " + c);

        c.addAssign(a);
        System.out.println("c += a : " + c);

        c.subtractAssign(a);
        System.out.println("c -= a : " + c);

        c.multiplyAssign(a);
        System.out.println("c *= a : " + c);

        c.divideAssign(a);
        System.out.println("c /= a : " + c);

        try (Scanner scanner = new Scanner(System.in)) {
            System.out.print("Enter a value for a: ");
            a = new FiniteField(scanner.nextInt());
        }
        System.out.println("New value for a: " + a);
    }
}
