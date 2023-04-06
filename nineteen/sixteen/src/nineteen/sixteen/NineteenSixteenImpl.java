package nineteen.sixteen;

class NineteenSixteenImpl {
	public static int[] phase(int[] signal) {
		int[] nextPhase = new int[signal.length];

		for (int i = 0; i < signal.length; i++) {
			int value = 0;
			for (int k = i; k < signal.length; k++) {
				int patternIndex = ((k + 1) / (i + 1)) % 4;
				if (patternIndex == 1) value += signal[k];
				else if (patternIndex == 3) value -= signal[k];
				else k += i;
			}
			nextPhase[i] = Math.abs(value) % 10;
		}
		return nextPhase;
	}

}
