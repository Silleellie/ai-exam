import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class SchemaRestructurerRegex {

    public static String buildFact(String factName, ArrayList<String> factAttributes) {

        StringBuilder fact = new StringBuilder(String.format("'%s'", factName));

        fact.append("(");
        String arguments = String.join(", ", factAttributes);
        fact.append(arguments);
        fact.append(").");

        return fact.toString();
    }

    public static ArrayList<String> extractTaxonomies(Scanner scan, ArrayList<String> superClassAttributes) {

        ArrayList<String> taxonomiesFacts = new ArrayList<>();
        boolean endTaxonomy = false;

        while(!endTaxonomy) {

            String line = scan.nextLine();

            // case in which the class has no attributes or subclasses (xml tag is closed on same line)
            Pattern pattern = Pattern.compile("<value.*name=\"(.*)\"/>");
            Matcher matcher = pattern.matcher(line);

            if (matcher.find()) {
                taxonomiesFacts.add(buildFact(matcher.group(1), superClassAttributes));
            }

            // case in which
            pattern = Pattern.compile("<value.*name=\"(.*)\">");
            matcher = pattern.matcher(line);

            if (matcher.find()) {
                processEntity(scan, matcher.group(1), "</value>", superClassAttributes);
            }

            // case in which
            pattern = Pattern.compile("</taxonomy>");
            matcher = pattern.matcher(line);

            if (matcher.find()) {
                endTaxonomy = true;
            }
        }

        return taxonomiesFacts;
    }

    public static ArrayList<String> extractAttributes(Scanner scan) {

        ArrayList<String> attributes = new ArrayList<>();

        while(scan.hasNext()) {
            String line = scan.nextLine();

            Pattern pattern = Pattern.compile("<attribute.*?name=\"([^\"]*)\".*?/>");
            Matcher matcher = pattern.matcher(line);

            if (matcher.find()) {
                attributes.add(matcher.group(1));
            }

            pattern = Pattern.compile("</attributes>");
            matcher = pattern.matcher(line);

            if (matcher.find()) {
                return attributes;
            }

        }

        return attributes;
    }

    public static ArrayList<String> processEntity(Scanner scan, String entityName, String closing, ArrayList<String> superClassAttributes) {

        ArrayList<String> facts = new ArrayList<>();
        ArrayList<String> entityAttributes = new ArrayList<>(superClassAttributes);
        ArrayList<String> entitySubClasses = new ArrayList<>();
        boolean endEntity = false;

        while(!endEntity) {

            String line = scan.nextLine();

            Pattern pattern = Pattern.compile("<attributes>");
            Matcher matcher = pattern.matcher(line);

            if (matcher.find()) {
                entityAttributes = extractAttributes(scan);
            }

            pattern = Pattern.compile("<taxonomy>");
            matcher = pattern.matcher(line);

            if (matcher.find()) {
                entitySubClasses = extractTaxonomies(scan, entityAttributes);
            }

            pattern = Pattern.compile(closing);
            matcher = pattern.matcher(line);

            if (matcher.find()) {
                endEntity = true;
            }

        }

        facts.add(buildFact(entityName, entityAttributes));
        facts.addAll(entitySubClasses);

        return facts;
    }

    public static void main (String[] args) {

        Scanner scan = null;
        try {
            scan = new Scanner(new File("tourism.xml"));
            ArrayList<String> allFacts = new ArrayList<>();

            while (scan.hasNext()) {
                String line = scan.nextLine();

                Pattern pattern = Pattern.compile("<entity name=\"(.*)\">");
                Matcher matcher = pattern.matcher(line);

                if (matcher.find()) {
                    allFacts.addAll(processEntity(scan, matcher.group(1), "</entity>", new ArrayList<String>()));
                }

            }

            scan.close();

            FileWriter kbWriter = new FileWriter("file.pl");

            for (String fact : allFacts) {
                kbWriter.write(fact + "\n");
            }

            kbWriter.close();

        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }
}
