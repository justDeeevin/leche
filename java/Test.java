class Test {
	public static void main(String[] args) {
		int a = 1;
		int b = 2;
		if (args.length == 2) {
			a = Integer.parseInt(args[0]);
			b = Integer.parseInt(args[1]);
		}
		System.out.println(add(a, b));
	}

	private static int add(int a, int b) {
		return a + b;
	}
}
