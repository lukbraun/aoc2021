package org.example;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class AdventOfCodeImpl {

    public static void main(String[] args) {
        String file = "input_test";
        if (args.length > 0) {
            file = args[0];
        }
        final List<NavigationSubsystemSyntaxChecker> syntaxChecker = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {
            for (String line; (line = br.readLine()) != null;) {
                // sumOfErrors += .getErrorScore();
                syntaxChecker.add(new NavigationSubsystemSyntaxChecker(line));
            }
        } catch (IOException e) {
            System.err.println("Error reading file");
        }
        System.out.println("Sum of Errors is: " + syntaxChecker.stream()
                .map(NavigationSubsystemSyntaxChecker::getErrorScore).reduce(0, (acc, element) -> acc + element));

        List<BigInteger> autoCompleteRes = syntaxChecker.stream()
                .filter(NavigationSubsystemSyntaxChecker::isNotCorrupted)
                .map(NavigationSubsystemSyntaxChecker::getAutocompleteScore)
                .collect(Collectors.toList());

        Collections.sort(autoCompleteRes);
        
        int middle = autoCompleteRes.size() / 2;

        if(autoCompleteRes.size() > 0) {
            System.out.println("Autocomplete Middle Value is: " + autoCompleteRes.get(middle));
        }
    }
}
