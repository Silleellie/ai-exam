package src_java;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class XMLEntityScanner {

    private class Entity {

        public String entityName;
        public ArrayList<HashMap<String, String>> attributes;
        public ArrayList<Entity> taxonomy;

        public Entity(String entityName, String entityContent) {

            this.entityName = entityName;
            this.attributes = extractAttributes(entityContent);
            this.taxonomy = extractTaxonomy(entityContent);

        }

        public ArrayList<HashMap<String, String>> extractAttributes(String entityContent) {

            ArrayList<HashMap<String, String>> attributes = new ArrayList<>();

            // add <specialSplitTag> in regex so to not have empty string at the beginning of split list
            Pattern attributesSectionPattern = Pattern.compile("<attributes><specialSplitTag>(.*)<\\/attributes>");
            Matcher attributesSectionMatcher = attributesSectionPattern.matcher(entityContent);

            if (attributesSectionMatcher.find()) {

                String allAttributes = attributesSectionMatcher.group(1);

                // all attributes are separated by <specialSplitTag> which we add as separator
                // to each XML tag when we load all entity content
                String[] attributesList = allAttributes.split("<specialSplitTag>");

                Pattern keyValuePattern = Pattern.compile("([^\\s=]+)=\"([^\"]*)\"");
                for (String singleAttribute : attributesList) {
                    Matcher keyValueMatcher = keyValuePattern.matcher(singleAttribute);

                    HashMap<String, String> attribute = new HashMap<>();
                    while (keyValueMatcher.find()) {

                        String key = keyValueMatcher.group(1);
                        String value = keyValueMatcher.group(2);

                        attribute.put(key, value);
                    }

                    attributes.add(attribute);
                }

            }

            return attributes;
        }

        public ArrayList<Entity> extractTaxonomy(String entityContent) {

            ArrayList<Entity> taxonomy = new ArrayList<>();

            Pattern taxonomySectionPattern = Pattern.compile("<taxonomy><specialSplitTag>(.*)<\\/taxonomy>");
            Matcher taxonomySectionMatcher = taxonomySectionPattern.matcher(entityContent);

            if (taxonomySectionMatcher.find()) {

                String allSubclasses = taxonomySectionMatcher.group(1);

                // all attributes are separated by <specialSplitTag> which we add as separator
                // to each XML tag when we load all entity content
                List<String> subclassesList = Arrays.asList(allSubclasses.split("<specialSplitTag>"));

                // value tag opened and closed
                Pattern simpleSubclassPattern = Pattern.compile("<value\\s*name=\"(.*)\"\\s*\\/>");
                // value tag only opened
                Pattern complexSubclassPattern = Pattern.compile("<value\\s*name=\"(.*)\"\\s*>");

                Iterator<String> itSubclass = subclassesList.iterator();

                while (itSubclass.hasNext()) {

                    String singleSubclass = itSubclass.next();

                    Matcher simpleSubclassMatcher = simpleSubclassPattern.matcher(singleSubclass);
                    Matcher complexSubclassMatcher = complexSubclassPattern.matcher(singleSubclass);

                    if (simpleSubclassMatcher.find()) {
                        String subclassName = simpleSubclassMatcher.group(1);

                        Entity simpleSubclass = new Entity(subclassName, "");

                        taxonomy.add(simpleSubclass);
                    } else if (complexSubclassMatcher.find()) {
                        String subclassName = complexSubclassMatcher.group(1);

                        // subclassContent is up to </value> token, not all the remaining list
                        String line = "";
                        StringBuilder subclassContent = new StringBuilder();
                        while(!line.equals("</value>")) {

                            line = itSubclass.next();
                            subclassContent.append(line);
                            subclassContent.append("<specialSplitTag>");

                        }

                        Entity complexSubclass = new Entity(subclassName, subclassContent.toString());

                        taxonomy.add(complexSubclass);
                    }

                }
            }

            return taxonomy;
        }

    }


    private final Scanner scan;

    public XMLEntityScanner(String xmlFilePath) throws FileNotFoundException {
        scan = new Scanner(new File(xmlFilePath));
    }

    public String gatherEntityName() {

        Pattern entityOpenPattern = Pattern.compile("<entity\\s*name=\"(.*)\"\\s*>");
        String entityName = null;

        while(entityName == null) {
            String line = scan.nextLine().trim();

            Matcher entityOpenMatcher = entityOpenPattern.matcher(line);

            if (entityOpenMatcher.find()) {
                entityName = entityOpenMatcher.group(1);
            }
        }

        return entityName;
    }


    public String gatherEntityContent() {

        StringBuilder entityContent = new StringBuilder();
        String line = "";

        while(!line.equals("</entity>")) {
            line = scan.nextLine().trim();

            entityContent.append(line);
            // special token which helps us to split different XML tag
            entityContent.append("<specialSplitTag>");
        }

        return entityContent.toString();
    }

    public Entity nextEntity(){

        String entityName = gatherEntityName();
        String entityContent = gatherEntityContent();

        return new Entity(entityName, entityContent);
    }


    public static void main (String[] args) {

        try {
            XMLEntityScanner a = new XMLEntityScanner("food_custom.xml");


            while (true) {
                Entity we = a.nextEntity();
                System.out.println("we");
            }


        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

//    @Override
//    public Iterator<Array> iterator() {
//        return new CustomIterator<Array>(this);
//    }
}

//class CustomIterator implements Iterator<Array> {
//
//    // constructor
//    CustomIterator<>(CustomDataStructure obj) {
//        // initialize cursor
//    }
//
//    // Checks if the next element exists
//    public boolean hasNext() {
//    }
//
//    // moves the cursor/iterator to next element
//    public T next() {
//    }
//
//    // Used to remove an element. Implement only if needed
//    public void remove() {
//        // Default throws UnsupportedOperationException.
//    }
//}