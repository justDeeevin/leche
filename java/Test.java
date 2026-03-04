class Test {
	public static void main(String[] args) {
		for (int i = 1; i <= 100; i++) {
			System.out.print(i + ": ");
			String s = new String();
			if (i % 3 == 0) {
				s += "fizz";
			}
			if (i % 5 == 0) {
				s += "buzz";
			}
			System.out.println(s);
		}
	}
}
