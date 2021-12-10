package org.example;

import java.util.Map;

public class ErrorCalculator {
    public static int calcWrongClosingParen(final char character) {
        return switch (character) {
            case ')', '(' -> 3;
            case ']', '[' -> 57;
            case '}', '{' -> 1197;
            case '>', '<' -> 25137;
            default -> 0;
        };
    }

    public static int calcMissingClosing(final char character) {
        return switch (character) {
            case ')', '(' -> 1;
            case ']', '[' -> 2;
            case '}', '{' -> 3;
            case '>', '<' -> 4;
            default -> 0;
        };
    }

    final static Map<Character, Character> openingClosingMap = Map.of('(', ')', '[', ']', '{', '}', '<', '>');

    public static boolean closingMatchesOpening(final char opening, final char closing) {
        return openingClosingMap.get(opening) == closing;
    }
}
