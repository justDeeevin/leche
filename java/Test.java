class Test {
	public static void main(String[] args) {
		int x = Integer.parseInt(args[0]);

		switch (x) {
			case 1:
				System.out.println("one");
				break;
			case 2:
				System.out.println("two");
				break;
			case 3:
				System.out.println("three");
				break;
			default:
				System.out.println("default");
				break;
		}
	}
}
