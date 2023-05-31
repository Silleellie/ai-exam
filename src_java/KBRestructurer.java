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


/**
 * Class responsible for restructuring the KB into a more appropriate Prolog formalism
 */
public class KBRestructurer {

    /**
     * Method to check if the given line contains a 'node_properties' or 'arc_properties' predicate in order to
     * extract its data (in particular, the predicate id and the properties' dictionary)
     *
     * @param line A single String line from the Prolog file containing all instances
     * @return result ArrayList of String containing in order:
     *  - predicate type ('node_properties' or 'arc_properties');
     *  - predicate id
     *  - properties dictionary
     */
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


    /**
     * Method to convert a single fact in the old Knowledge Base into the 'list' based formalism.
     * <p>
     * EX:
     *      node_properties(0, '{name=Jack, gender=M}'). ---> node_properties(0, ['name'-Jack, 'gender'-M]).
     *      arc_properties(1, '{date=11/05/1998}'). ---> arc_properties(1, ['date'-'11/05/1998']).
     *
     * @param metaPredicate either 'node_properties' or 'arc_properties'
     * @param dict String dictionary containing the properties (ex: '{name=Jack, gender=M}')
     * @param predicateId String ID of the predicate
     * @return single prolog fact in 'list' representation form
     */
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

    /**
     * Method to convert a single fact in the old Knowledge Base into the 'list' based formalism.
     * <p>
     * EX:
     *      node_properties(0, '{name=Jack, gender=M}'). ---> name(0, Jack). gender(0, M).
     *      arc_properties(1, '{date=11/05/1998}'). ---> date(1, '11/05/1998').
     *
     * @param dict String dictionary containing the properties (ex: '{name=Jack, gender=M}')
     * @param predicateId String ID of the predicate
     * @return multiple prolog facts in 'facts' representation form
     */
    private static String convertLineToFacts(String dict, String predicateId) {

        ArrayList<String> facts = new ArrayList<>();

        Pattern pattern = Pattern.compile("(\\w+)=((?:[^=,]|,)*(?=}|,\\s*\\w+=))");
        Matcher matcher = pattern.matcher(dict);

        while (matcher.find()) {

            String matchKey = matcher.group(1);
            String matchValue = matcher.group(2);

            String fact = "%s(%s, '%s').".formatted(matchKey, predicateId, matchValue);
            facts.add(fact);

        }

        return String.join("\n", facts);
    }

    /**
     * Optional step of the restructuring pipeline. Since no 'arc_properties' are defined in the original instances file,
     * this allows to split 'arc' predicates specified in the file to 'arc' and 'arc_properties' predicates, so to make
     * more homogeneous the KB to restructure.
     * <p>
     *     ex: arc(1, 'knows', 0, 6). -> arc(1, 0, 6). arc_properties(1, '{SubClass=knows}').
     * <p>
     * This is a preliminary process with respect to the whole restructuring process, so this substitution is applied
     * directly to the old knowledge base file and the results stored in a temporary file.
     *
     * @param pathOldKB String path were the old Knowledge Base to restructure is stored
     */
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

    /**
     * Method which handles and controls the restructuring process of the KB. Iterates over the lines of the old
     * knowledge base and processes them accordingly. The restructured line is then written into the new knowledge
     * base.
     *
     * @param mode either 'list' or 'facts'
     * @param pathOldKB String path were the old Knowledge Base to restructure is stored
     * @param pathNewKB String path were the new restructured Knowledge Base will be stored
     */
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


    /**
     * Method which retrieves the knowledge base to modify from the 'inputs' folder (by searching for files with '.pl' extension).
     * Three cases are considered:
     *  - No files were found, in which case a FileNotFoundException is raised;
     *  - One file was found, in which case its path is retrieved and returned;
     *  - More than one file was found, in which case the user is prompted to select one path out of all the ones that were found.
     *
     * @param inputScanner Scanner used to interact with the user in case of multiple possible KBs
     * @return the file path associated with the selected KB to restructure
     * @throws FileNotFoundException if no elegible KB files are available in the 'inputs' folder
     */
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

    /**
     * Method to retrieve from the user the desired mode into which the KB will be restructured. Two cases are
     * possible:
     *  - "list": all procedures will be converted in the 'list' formalism
     *  - "facts": all procedures will be converted in the 'facts' formalism
     *
     * @param inputScanner Scanner used to interact with the user to retrieve the desired modality
     * @return either "list" or "facts"
     */
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

    /**
     * Method to retrieve from the user the choice whether they want to restructure the 'arc' procedures in the knowledge base
     * to split them into 'arc' and 'arc_properties' or not.
     *
     * @param inputScanner Scanner used to interact with the user to retrieve their choice
     * @return either "y" or "n"
     */
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


    /**
     * Method to call to start the restructure from the old KB file to a more appropriate Prolog formalism.
     * Before actually performing the whole restructuring of the KB, an intermediate restructure for the "arc"
     * predicates can (and must) be performed to make the KB more homogeneous, so to have also for arcs the
     * "arc_properties" predicate which has as argument all attributes of the particular arc
     * (just like the "node_properties" predicate).
     * <p>
     * The restructured KB will be saved in the 'outputs' folder.
     * If more than one eligible KB file is found, the user is prompted to choose the appropriate one
     *
     * @throws IOException If an I/O error occurs
     */
    public static void main(String[] args) throws IOException {

        Scanner input = new Scanner(System.in);

        String inputFilename = getInputFilename(input);
        String restructureArcs = getRestructureArcs(input);
        String mode = getMode(input);

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
