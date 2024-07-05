public class Test {
    public static void main(String[] args) {
        FiniteField message = new FiniteField(1337);

        int testedModulo = 1234567891;
        DHSetup<FiniteField> setup = new DHSetup<>(testedModulo);

        User<FiniteField> user1 = new User<>(setup);
        User<FiniteField> user2 = new User<>(setup);

        user2.setKey(user1.getPublicKey());
        user1.setKey(user2.getPublicKey());

        System.out.println("Finite Galois field set to: 1234567891");

        System.out.println("Generator: " + setup.getGenerator());

        System.out.println("Message: " + message);

        FiniteField cipher1 = user1.encrypt(message);
        FiniteField cipher2 = user2.encrypt(message);

        System.out.println("Encrypted message: " + cipher1);

        FiniteField result1 = user1.decrypt(cipher2);
        FiniteField result2 = user2.decrypt(cipher1);

        System.out.println("Decrypted user1: " + result1);
        System.out.println("Decrypted user2: " + result2);
    }
}