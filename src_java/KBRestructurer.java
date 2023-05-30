package src_java;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

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


    private static String getInputFilename(Scanner inputScanner) throws FileNotFoundException {

        List<String> result;
        String inputFilename;

        try (Stream<Path> walk = Files.walk(Paths.get("inputs"))) {
            result = walk
                    .filter(p -> !Files.isDirectory(p))   // not a directory
                    .map(Path::toString)                  // convert path to string
                    .filter(f -> f.endsWith("pl"))        // check end with
                    .toList();                            // collect all matched to a List

        } catch (IOException e) {
            throw new FileNotFoundException("No Prolog file containing the exported graph found in 'inputs' folder!");
        }

        if (result.size() > 1) {

            System.out.printf("Found %d possible KB files! Please choose the correct one to use:%n", result.size());

            for (int i = 0; i < result.size(); i++) {

                Path pathToFile = Paths.get(result.get(i));
                String fileName = String.valueOf(pathToFile.getFileName());

                System.out.printf("%d ---> %s%n", i + 1, fileName);

            }

            int choice = 0;

            do {

                try {
                    choice = Integer.parseInt(inputScanner.nextLine());

                    if (choice <= 0 || choice > result.size()) {
                        throw new NumberFormatException();
                    }

                } catch (NumberFormatException e) {
                    System.out.println("The inserted integer is not valid, please insert a valid number");
                }

            } while (choice <= 0 || choice > result.size());

            inputFilename = result.get(choice - 1);

        } else {

            inputFilename = result.get(0);

            Path pathToFile = Paths.get(result.get(0));
            String fileName = String.valueOf(pathToFile.getFileName());

            System.out.printf("Found one possible KB file: %s will be used%n", fileName);

        }

        return inputFilename;
    }


    private static String getMode(Scanner inputScanner) {

        HashSet<String> validValues = new HashSet<>(Arrays.asList("list", "facts"));

        System.out.printf("Please insert conversion mode. Values accepted: %s%n", validValues);

        String mode;

        do {

            mode = inputScanner.nextLine();

            if (!mode.equals("list") && !mode.equals("facts")) {
                System.out.printf("The inserted input is not valid! Values accepted %s%n", validValues);
            }

        } while (!mode.equals("list") && !mode.equals("facts"));

        return mode;

    }

    private static String getRestructureArcs(Scanner inputScanner) {

        String restructureArcs;

        System.out.printf("Please choose whether arc predicates should be restructured in arc properties: \"y/n\"%n");

        do {

            restructureArcs = inputScanner.nextLine();

            if (!restructureArcs.equals("y") && !restructureArcs.equals("n")) {
                System.out.println("The inserted input is not valid! Please answer with 'y' or 'n'");
            }

        } while (!restructureArcs.equals("y") && !restructureArcs.equals("n"));

        return restructureArcs;
    }


    public static void main(String[] args) throws FileNotFoundException {

        Scanner input = new Scanner(System.in);

        String inputFilename = getInputFilename(input);
        String mode = getMode(input);
        String restructureArcs = getRestructureArcs(input);

        String outputFilename = "outputs/%s_%s".formatted(mode, new File(inputFilename).getName());

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
