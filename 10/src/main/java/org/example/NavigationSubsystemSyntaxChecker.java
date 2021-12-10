package org.example;

import java.math.BigInteger;
import java.util.Stack;

public class NavigationSubsystemSyntaxChecker {

    private int errorScore;

    private BigInteger autocompleteScore;

    public NavigationSubsystemSyntaxChecker(final String line) {
        checkLine(line);
    }

    public int getErrorScore() {
        return errorScore;
    }

    public BigInteger getAutocompleteScore() {
        return autocompleteScore;
    }

    public boolean isNotCorrupted() {
        return errorScore == 0;
    }

    private void checkLine(final String line) {
        final Stack<Character> characterStack = new Stack<>();
        for (char character : line.toCharArray()) {
            if (isOpening(character)) {
                characterStack.add(character);
            } else if (isClosing(character)) {
                final char currentOpening = characterStack.pop();
                if (!ErrorCalculator.closingMatchesOpening(currentOpening, character)) {
                    errorScore = ErrorCalculator.calcWrongClosingParen(character);
                    return;
                }
            }
        }
        errorScore = 0;
        this.autocompleteScore = BigInteger.valueOf(0);
        while(!characterStack.empty()) {
            final char current = characterStack.pop();
            this.autocompleteScore = this.autocompleteScore.multiply(BigInteger.valueOf(5)).add(BigInteger.valueOf(ErrorCalculator.calcMissingClosing(current)));
        }
    }

    private static boolean isOpeningRound(final char character) {
        return character == '(';
    }

    private static boolean isClosingRound(final char character) {
        return character == ')';
    }

    private static boolean isOpeningSquare(final char character) {
        return character == '[';
    }

    private static boolean isClosingSquare(final char character) {
        return character == ']';
    }

    private static boolean isOpeningCurly(final char character) {
        return character == '{';
    }

    private static boolean isClosingCurly(final char character) {
        return character == '}';
    }

    private static boolean isOpeningAngle(final char character) {
        return character == '<';
    }

    private static boolean isClosingAngle(final char character) {
        return character == '>';
    }

    private static boolean isOpening(final char character) {
        return isOpeningAngle(character) || isOpeningCurly(character) || isOpeningRound(character)
                || isOpeningSquare(character);
    }

    private static boolean isClosing(final char character) {
        return isClosingAngle(character) || isClosingCurly(character) || isClosingRound(character)
                || isClosingSquare(character);
    }
}
