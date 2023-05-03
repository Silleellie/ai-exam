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
        Pattern pattern = Pattern.compile("node_properties\\(([0-9]+),\\s*'(.+)'\\)\\.");
        Matcher matcher = pattern.matcher(line);

        ArrayList<String> result = new ArrayList<>();

        if (matcher.find()) {
            result.add(matcher.group(1));
            result.add(matcher.group(2));
        }

        return result;
    }


    private static String convertLineToList(String dict, String predicateId) {

        StringBuilder list = new StringBuilder("[");

        Pattern pattern = Pattern.compile("(\\w+)=((?:[^=,]|,)*(?=}|,\\s*\\w+=))");
        Matcher matcher = pattern.matcher(dict);

        while (matcher.find()) {

            String matchKey = matcher.group(1);
            String matchValue = matcher.group(2);

            list.append(matchKey);
            list.append("-'");
            list.append(matchValue);
            list.append("', ");
        }

        list.delete(list.length() - 2, list.length());

        list.append("]");

        return "node_properties(%s, %s)".formatted(predicateId, list.toString());

    }


    private static String convertLineToFacts(String dict, String predicateId) {

        StringBuilder facts = new StringBuilder();

        Pattern pattern = Pattern.compile("(\\w+)=((?:[^=,]|,)*(?=}|,\\s*\\w+=))");
        Matcher matcher = pattern.matcher(dict);

        while (matcher.find()) {

            String matchKey = matcher.group(1);
            String matchValue = matcher.group(2);

            String fact = String.format("%s(%s, '%s').\n", matchKey, predicateId, matchValue);
            facts.append(fact);

        }

        facts.delete(facts.length() - 1, facts.length());

        return facts.toString();

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

                    String predicateId = predicate.get(0);
                    String properties = predicate.get(1);
                    if (mode.equals("list")) {
                        data = convertLineToList(properties, predicateId);
                    } else {
                        data = convertLineToFacts(properties, predicateId);
                    }

                }

                kbWriter.write(data + "\n");
                processedLines++;

                if ((processedLines % 100000) == 0) {
                    System.out.printf("Processed %d lines%n", processedLines);
                }
            }

            fileIter.close();

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


        System.out.println("Please insert the filename of the KB to restructure");
        String inputFilename = input.nextLine();

        System.out.printf("Please insert conversion mode. Values accepted: %s%n", validValues);
        String mode = input.nextLine();


        if (!(validValues.contains(mode))) {
            throw new IllegalArgumentException(("Value %s is not supported! " +
                    "Valid values: %s").formatted(mode, validValues.toString()));
        }

        String outputFilename = "%sExportedGraph.pl".formatted(mode);

        convertFile(mode, inputFilename, outputFilename);

        String outputPath = FileSystems.getDefault().getPath(outputFilename).toAbsolutePath().toString();

        System.out.printf("Converted Prolog file saved into %s!%n", outputPath);
    }
}
