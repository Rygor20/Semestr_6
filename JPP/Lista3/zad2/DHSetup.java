import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.Random;

class DHSetup<T extends FiniteField> {
    private T a;
    private int modulo;

    private boolean check(long number) {
		long i = 2;
		long tmp = number;

		while (i * i <= tmp) {
			if (tmp % i == 0) {
				if (Math.pow(number, (modulo - 1) / i) == 1) {
					return false;
				}
				tmp /= i;
			} else {
				i++;
			}
		}
		if (tmp > 1 && Math.pow(number, (modulo - 1) / tmp) == 1) {
			return false;
		}
		return true;
	}

    public DHSetup(int characteristic) {
        modulo = characteristic;
        
        Random rand = new Random();
        int newVal;
        T newA;

        do {
            newVal = 1 + rand.nextInt(modulo-1);

        } while (!check(newVal));
        this.a = (T) new FiniteField(newVal);
    }

    public T power(T number, long b) {
		T result = (T) new FiniteField(1);
		while (b > 0) {
			if (b % 2 == 1) {
				result = (T) result.multiply(number);
			}
			number = (T) number.multiply(number);
			b >>= 1;
		}
		return result;
	}

    public T getGenerator() {
        return a;
    }
}
