package src_java;

import src_java.xmlschemaelement.Entity;
import src_java.xmlschemaelement.Relationship;
import src_java.xmlschemaelement.XMLSchemaScanner;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Scanner;


public class SchemaToProlog {

    public static void main (String[] args) throws IOException {

        System.out.println("Please insert the filename of the XML schema to convert:");
        Scanner input = new Scanner(System.in);
        String inputFilename = input.nextLine();

        // if input filename has extension, strip it and then append .pl
        String outputFilename = (inputFilename.contains(".")) ?
                inputFilename.substring(0, inputFilename.lastIndexOf('.')) :
                inputFilename;
        outputFilename = "%s.pl".formatted(outputFilename);

        XMLSchemaScanner xml = new XMLSchemaScanner(inputFilename);

        FileWriter kbWriter = new FileWriter(outputFilename);

        kbWriter.write("% ENTITIES\n\n");

        Iterator<Entity> itEntities = xml.iteratorEntities();
        while (itEntities.hasNext()) {

            Entity originalClass = itEntities.next();

            // prologFact will contain facts for all the various subclasses separated by \n
            String prologFact = String.join("\n", obtainPrologFact(originalClass));
            kbWriter.write(prologFact);

            // double visual separator for different taxonomies
            kbWriter.write("\n");
            kbWriter.write("\n");
        }

        kbWriter.write("\n\n% RELATIONSHIPS\n\n");

        Iterator<Relationship> itRelationships = xml.iteratorRelationships();
        while (itRelationships.hasNext()) {

            Relationship originalRelationship = itRelationships.next();
            String prologFact = obtainPrologFact(originalRelationship);
            kbWriter.write(prologFact + "\n");

            // write inverse relationship
            if (originalRelationship.inverseRelationship != null) {
                kbWriter.write(obtainPrologFact(originalRelationship.inverseRelationship) + "\n");
            }

        }

        kbWriter.close();

        String outputPath = FileSystems.getDefault().getPath(outputFilename).toAbsolutePath().toString();
        System.out.printf("Converted schema saved into %s!%n", outputPath);
    }

    public static ArrayList<String> obtainPrologFact(Entity xmlEntity) {

        ArrayList<String> factList = new ArrayList<>();

        // wrap name with quotes to respect format of class name (mantain "/", first upper letter, ...)
        String predicateName = "'%s'".formatted(xmlEntity.name);

        // extracting attributes name
        ArrayList<String> attributeList = new ArrayList<>();

        for (HashMap<String, Object> attribute : xmlEntity.attributes) {
            attributeList.add((String) attribute.get("name"));

        }

        String arguments = "[%s]".formatted(String.join(", ", attributeList));
        String originalFact = "%s(%s).".formatted(predicateName, arguments);

        factList.add(originalFact);

        for (Entity subClass : xmlEntity.taxonomy) {

            factList.addAll(obtainPrologFact(subClass));

        }

        return factList;
    }

    public static String obtainPrologFact(Relationship xmlEntity) {

        // wrap name with quotes to respect format of class name (mantain "/", first upper letter, ...)
        String predicateName = "'%s'".formatted(xmlEntity.name);


        // extracting references (subject-object) pair
        ArrayList<String> referenceList = new ArrayList<>();

        for (HashMap<String, String> references : xmlEntity.references) {
            String subj = references.get("subject");
            String obj = references.get("object");

            // wrap in quotes class names to preserve formatting
            referenceList.add("'%s'-'%s'".formatted(subj, obj));
        }

        String referenceArgument = "[%s]".formatted(String.join(", ", referenceList));


        // extracting attributes name
        ArrayList<String> attributeList = new ArrayList<>();

        for (HashMap<String, Object> attribute : xmlEntity.attributes) {
            attributeList.add((String) attribute.get("name"));
        }

        String attributeArguments = "[%s]".formatted(String.join(", ", attributeList));

        String arguments = "%s, %s".formatted(referenceArgument, attributeArguments);

        return "%s(%s).".formatted(predicateName, arguments);
    }

}
