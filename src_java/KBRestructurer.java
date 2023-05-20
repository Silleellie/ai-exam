package src_java;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class KBRestructurer {


    private static ArrayList<String> validPredicate(String line) {
        Pattern pattern = Pattern.compile("(node_properties|arc_properties)\\(([0-9]+),\\s*'(.+)'\\)\\.");
        Matcher matcher = pattern.matcher(line);

        ArrayList<String> result = new ArrayList<>();

        if (matcher.find()) {
            result.add(matcher.group(1));  // node_properties or arc_properties
            result.add(matcher.group(2));  // predicate_id
            result.add(matcher.group(3));  // string dict representing properties e.g. '{name=Charles Ingerham, ...}'
        }

        return result;
    }


    private static String convertLineToList(String metaPredicate, String dict, String predicateId) {

        Pattern pattern = Pattern.compile("(\\w+)=((?:[^=,]|,)*(?=}|,\\s*\\w+=))");
        Matcher matcher = pattern.matcher(dict);

        ArrayList<String> keyValList = new ArrayList<>();

        while (matcher.find()) {

            String matchKey = matcher.group(1);
            String matchValue = matcher.group(2);

            // '' quotes around key and value to preserve formatting
            keyValList.add("'%s'-'%s'".formatted(matchKey, matchValue));
        }

        String arguments = "[%s]".formatted(String.join(", ", keyValList));

        return "%s(%s, %s).".formatted(metaPredicate, predicateId, arguments);

    }


    private static String convertLineToFacts(String dict, String predicateId) {

        ArrayList<String> facts = new ArrayList<>();

        Pattern pattern = Pattern.compile("(\\w+)=((?:[^=,]|,)*(?=}|,\\s*\\w+=))");
        Matcher matcher = pattern.matcher(dict);

        while (matcher.find()) {

            String matchKey = matcher.group(1);
            String matchValue = matcher.group(2);

            String fact = "%s('%s', '%s').".formatted(matchKey, predicateId, matchValue);
            facts.add(fact);

        }

        return String.join("\n", facts);
    }

    private static void convertArcToArcProperties(String pathOldKB) {
        try {
            System.out.println("*************** Restructuring arc in arc properties started ***************");
            File kbFile = new File(pathOldKB);
            FileWriter kbWriter = new FileWriter("temp_kb.pl");

            Scanner fileIter = new Scanner(kbFile);

            int processedLines = 0;
            while (fileIter.hasNextLine()) {

                String line = fileIter.nextLine();

                Pattern pattern = Pattern.compile("arc\\(([0-9]+),\\s*'(.+)',\\s*([0-9]+),\\s*([0-9]+)\\)\\.");
                Matcher matcher = pattern.matcher(line);

                if (matcher.find()) {

                    String predicateId = matcher.group(1);
                    String relationshipName = matcher.group(2);
                    String subjectId = matcher.group(3);
                    String objectId = matcher.group(4);

                    String arcFact = "arc(%s, %s, %s).".formatted(predicateId, subjectId, objectId);
                    String arcPropertiesFact = "arc_properties(%s, '{subClass=%s}').".formatted(predicateId, relationshipName);

                    line = "%s\n%s".formatted(arcFact, arcPropertiesFact);

                }

                kbWriter.write(line + "\n");
                processedLines++;

                if ((processedLines % 100000) == 0) {
                    System.out.printf("Processed %d lines%n", processedLines);
                }

            }

            System.out.printf("Arc facts restructured! Now whole file restructuring will start %n%n");

            fileIter.close();
            kbWriter.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    private static void convertFile(String mode, String pathOldKB, String pathNewKB) {
        try {
            System.out.println("*************** Restructure started ***************");

            File kbFile = new File(pathOldKB);
            FileWriter kbWriter = new FileWriter(pathNewKB);

            Scanner fileIter = new Scanner(kbFile);

            int processedLines = 0;
            while (fileIter.hasNextLine()) {

                String data = fileIter.nextLine();

                ArrayList<String> predicate = validPredicate(data);

                if (predicate.size() != 0) {

                    String metaPredicate = predicate.get(0);
                    String predicateId = predicate.get(1);
                    String propertiesString = predicate.get(2);

                    if (mode.equals("list")) {
                        data = convertLineToList(metaPredicate, propertiesString, predicateId);
                    } else {
                        data = convertLineToFacts(propertiesString, predicateId);
                    }

                    // one additional space for better visualizing predicates of different predicateId
                    data = "%s\n".formatted(data);
                }

                kbWriter.write(data + "\n");
                processedLines++;

                if ((processedLines % 100000) == 0) {
                    System.out.printf("Processed %d lines%n", processedLines);
                }
            }

            fileIter.close();
            kbWriter.close();

        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {

        HashSet<String> validValues = new HashSet<>(Arrays.asList("list", "facts"));
        Scanner input = new Scanner(System.in);


        System.out.println("Please insert the filename of the KB to restructure:");
        String inputFilename = input.nextLine();

        System.out.printf("Please insert conversion mode. Values accepted: %s%n", validValues);
        String mode = input.nextLine();

        System.out.printf("Please choose whether arc predicates should be restructured in arc properties: \"y/n\"%n");
        String restructureArcs = input.nextLine();


        if (!(validValues.contains(mode))) {
            throw new IllegalArgumentException(("Value %s is not supported! " +
                    "Valid values: %s").formatted(mode, validValues.toString()));
        }

        String outputFilename = "%s%s".formatted(mode, inputFilename);

        if (restructureArcs.equals("y")) {
            convertArcToArcProperties(inputFilename);
            convertFile(mode, "temp_kb.pl", outputFilename);
            new File("temp_kb.pl").delete();
        } else {
            convertFile(mode, inputFilename, outputFilename);
        }

        String outputPath = FileSystems.getDefault().getPath(outputFilename).toAbsolutePath().toString();

        System.out.printf("Converted Prolog file saved into %s!%n", outputPath);
    }
}
