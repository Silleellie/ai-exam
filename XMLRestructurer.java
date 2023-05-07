import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

import javax.xml.parsers.*;
import org.xml.sax.*;
import org.w3c.dom.*;

public class XMLRestructurer {

    private static String getParentNodeAttributesToString(ArrayList<String> parentNodeAttributes) {

        String toWrite;
        StringBuilder attributes = new StringBuilder();

        for (String parentNodeAttribute : parentNodeAttributes) {
            toWrite = parentNodeAttribute + ", ";
            attributes.append(toWrite);
        }

        return attributes.toString();

    }

    private static String generateStringRelationshipFact(String name, String references, String attributes) {

        StringBuilder fact = new StringBuilder();
        String factString;
        fact.append(name);
        fact.append("(");

        if (references.length() != 2 && references.length() != 0) {
            fact.append(references);
        }

        if (attributes.length() != 2 && attributes.length() != 0) {

            if (references.length() != 2 && references.length() != 0) {
                fact.append(", ");
            }

            fact.append(attributes);
        }

        fact.append(").\n");

        factString = fact.toString();

        fact.setLength(0);
        fact.trimToSize();

        return factString;
    }

    private static ArrayList<String> inspectRelationships(String parentNodeName, String inverseName, NodeList nodes) {

        ArrayList<String> factsStrings = new ArrayList<>();
        StringBuilder attributes = new StringBuilder();
        StringBuilder references = new StringBuilder();
        StringBuilder inverseReferences = new StringBuilder();
        String toWrite;
        HashMap<String, Integer> mapping = new HashMap<>();

        for (int j = 1; j < nodes.getLength(); j++) {

            if (nodes.item(j).getNodeName().equals("references")) {
                mapping.put("references", j);
            } else if (nodes.item(j).getNodeName().equals("attributes")) {
                mapping.put("attributes", j);
            }

        }

        if (mapping.containsKey("references")) {

            references.append("[");
            inverseReferences.append("[");

            Integer attributesIndex = mapping.get("references");
            NodeList entityAttributes = nodes.item(attributesIndex).getChildNodes();

            for (int j = 1; j < entityAttributes.getLength(); j += 2) {

                String subjectName = entityAttributes.item(j).getAttributes().getNamedItem("subject").getNodeValue().toLowerCase();
                String objectName = entityAttributes.item(j).getAttributes().getNamedItem("object").getNodeValue().toLowerCase();
                toWrite = subjectName + "-" + objectName + ", ";
                references.append(toWrite);
                toWrite = objectName + "-" + subjectName + ", ";
                inverseReferences.append(toWrite);

            }

            references.delete(references.length() - 2, references.length());
            references.append("]");

            inverseReferences.delete(inverseReferences.length() - 2, inverseReferences.length());
            inverseReferences.append("]");

        }

        if (mapping.containsKey("attributes")) {

            attributes.append("[");

            Integer attributesIndex = mapping.get("attributes");
            NodeList entityAttributes = nodes.item(attributesIndex).getChildNodes();

            for (int j = 1; j < entityAttributes.getLength(); j += 2) {

                String attributeName = entityAttributes.item(j).getAttributes().getNamedItem("name").getNodeValue();
                toWrite = attributeName + ", ";
                attributes.append(toWrite);

            }

            attributes.delete(attributes.length() - 2, attributes.length());
            attributes.append("]");

        }

        String referencesString = references.toString();
        String inverseReferencesString = inverseReferences.toString();
        String attributesString = attributes.toString();

        attributes.setLength(0);
        attributes.trimToSize();

        references.setLength(0);
        references.trimToSize();

        inverseReferences.setLength(0);
        inverseReferences.trimToSize();

        String parentNodeNameToWrite = "'" + parentNodeName + "'";

        factsStrings.add(generateStringRelationshipFact(parentNodeNameToWrite, referencesString, attributesString));

        if (!inverseName.equals(parentNodeName)) {
            factsStrings.add(generateStringRelationshipFact(parentNodeNameToWrite, inverseReferencesString, attributesString));
        }

        return factsStrings;

    }

    private static ArrayList<String> inspectEntities(String nodeName, ArrayList<String> parentNodeAttributes, NodeList nodes) {

        ArrayList<String> factsStrings = new ArrayList<>();
        StringBuilder fact = new StringBuilder();
        String toWrite;
        HashMap<String, Integer> mapping = new HashMap<>();

        toWrite = "'" + nodeName + "'(";
        fact.append(toWrite);

        if (parentNodeAttributes.size() != 0) {

            fact.append("[");
            fact.append(getParentNodeAttributesToString(parentNodeAttributes));

        }

        for (int j = 1; j < nodes.getLength(); j++) {

            if (nodes.item(j).getNodeName().equals("attributes")) {
                mapping.put("attributes", j);
            } else if (nodes.item(j).getNodeName().equals("taxonomy")) {
                mapping.put("taxonomy", j);
            }

        }

        ArrayList<String> classAttributes = new ArrayList<>(parentNodeAttributes);

        if (mapping.containsKey("attributes")) {

            if (parentNodeAttributes.size() == 0) {
                fact.append("[");
            }

            Integer attributesIndex = mapping.get("attributes");
            NodeList entityAttributes = nodes.item(attributesIndex).getChildNodes();

            for (int j = 1; j < entityAttributes.getLength(); j += 2) {

                String attributeName = entityAttributes.item(j).getAttributes().getNamedItem("name").getNodeValue();
                classAttributes.add(attributeName);
                toWrite = attributeName + ", ";
                fact.append(toWrite);

            }

            fact.delete(fact.length() - 2, fact.length());
            fact.append("]).");
            factsStrings.add(fact.toString() + "\n");

        } else if (parentNodeAttributes.size() != 0) {

            fact.delete(fact.length() - 2, fact.length());
            fact.append("]).");
            factsStrings.add(fact.toString() + "\n");

        }

        fact.setLength(0);
        fact.trimToSize();

        if (mapping.containsKey("taxonomy")) {

            Integer taxonomyIndex = mapping.get("taxonomy");
            NodeList entityTaxonomies = nodes.item(taxonomyIndex).getChildNodes();

            for (int j = 1; j < entityTaxonomies.getLength(); j += 2) {

                String childClassName = entityTaxonomies.item(j).getAttributes().getNamedItem("name").getNodeValue();
                NodeList entityAttributeChildNodes = entityTaxonomies.item(j).getChildNodes();
                factsStrings.addAll(inspectEntities(childClassName, classAttributes, entityAttributeChildNodes));

            }

        }

        return factsStrings;
    }

    private static void convertFile(String pathOriginalXML, String pathNewPL) {

        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

        try {

            FileWriter kbWriter = new FileWriter(pathNewPL);

            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.parse(pathOriginalXML);

            NodeList entities = document.getElementsByTagName("entity");

            for (int i = 0; i < entities.getLength(); i++) {

                String entityName = entities.item(i).getAttributes().getNamedItem("name").getNodeValue();
                NodeList entityAttributeChildNodes = entities.item(i).getChildNodes();
                ArrayList<String> factsToWrite = inspectEntities(entityName, new ArrayList<>(), entityAttributeChildNodes);

                for (String fact: factsToWrite) {
                    kbWriter.write(fact);
                }
            }

            NodeList relationships = document.getElementsByTagName("relationship");

            for (int i = 0; i < relationships.getLength(); i++) {

                String relationshipName = relationships.item(i).getAttributes().getNamedItem("name").getNodeValue();
                String relationshipInverseName = relationships.item(i).getAttributes().getNamedItem("inverse").getNodeValue();
                NodeList relationshipAttributeChildNodes = relationships.item(i).getChildNodes();
                ArrayList<String> factsToWrite = inspectRelationships(relationshipName, relationshipInverseName, relationshipAttributeChildNodes);

                for (String fact: factsToWrite) {
                    kbWriter.write(fact);
                }
            }

            kbWriter.close();

        } catch (ParserConfigurationException | SAXException | IOException err) {
            System.out.println("ERROR");
            System.out.println(err.getMessage());
        }

    }

    public static void main(String[] args) {

        Scanner input = new Scanner(System.in);

        System.out.println("Please insert the filename of the XML schema to convert");
        String inputFileName = input.nextLine();

        Path filePath = Paths.get(inputFileName);
        String fileDir = filePath.getParent().toString();
        String fileName = filePath.getFileName().toString();
        String fileNameWithOutExt = fileName.replaceFirst("[.][^.]+$", "");
        String newFileName = fileNameWithOutExt + ".pl";
        Path outputFileName = Paths.get(fileDir, newFileName);

        convertFile(inputFileName, outputFileName.toString());

        System.out.printf("Converted Prolog file saved into %s!%n", outputFileName);
    }

}
