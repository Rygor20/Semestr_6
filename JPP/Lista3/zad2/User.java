import java.util.Random;
import java.util.Optional;

class User<T extends FiniteField> {
    private int secret;
    private DHSetup<T> setup;
    private Optional<T> key;

    public User(DHSetup<T> newSetup) {
        Random rand = new Random();

		setup = newSetup;
        secret = rand.nextInt(1234567890);
    }

    public T getPublicKey() {
        return (T) setup.power(setup.getGenerator(), secret);
    }

    public void setKey(T a) {
        key = Optional.of(setup.power(a, secret));
    }

    public T encrypt(T m) {
        if (!key.isPresent()) {
			throw new IllegalStateException("Key not set");
		}
		return (T) m.multiply(key.get());
    }

    public T decrypt(T c) {
        if (!key.isPresent()) {
			throw new IllegalStateException("Key not set");
		}
		return (T) c.divide(key.get());
    }
}
