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
        String outputFilenameKb = (inputFilename.contains(".")) ?
                inputFilename.substring(0, inputFilename.lastIndexOf('.')) :
                inputFilename;
        outputFilenameKb = "%s.pl".formatted(outputFilenameKb);
        
        XMLSchemaScanner xml = new XMLSchemaScanner(inputFilename);
        FileWriter kbWriter = new FileWriter(outputFilenameKb);

        writeDirectives(kbWriter);
        writeEntities(xml, kbWriter);
        writeRelationships(xml, kbWriter);
        writeRules(kbWriter);

        kbWriter.close();

        String outputPath = FileSystems.getDefault().getPath(outputFilenameKb).toAbsolutePath().toString();
        System.out.printf("Converted schema saved into %s!%n", outputPath);

    }

    public static void writeDirectives(FileWriter kbWriter) throws IOException {

        kbWriter.write(":- use_module(library(lists)).\n");
        kbWriter.write(":- discontiguous inverse_of/2.\n\n\n");

    }

    public static void writeEntities(XMLSchemaScanner xml, FileWriter kbWriter) throws IOException {

        StringBuilder subclassOfFacts = new StringBuilder();
        StringBuilder entitiesFacts = new StringBuilder();

        Iterator<Entity> itEntities = xml.iteratorEntities();
        while (itEntities.hasNext()) {

            Entity originalClass = itEntities.next();

            // prologFact will contain facts for all the various subclasses separated by \n

            HashMap<String, ArrayList<String>> facts = obtainPrologFact(originalClass);

            subclassOfFacts.append(String.join("\n", facts.get("subclassOfFacts")));
            entitiesFacts.append(String.join("\n", facts.get("entityFacts")));

            // double visual separator for different taxonomies
            if (facts.get("subclassOfFacts").size() != 0) {
                subclassOfFacts.append("\n\n");
            }

            entitiesFacts.append("\n\n");
        }

        kbWriter.write("% ENTITIES\n\n");

        kbWriter.write("% Entities hierarchy\n\n");
        kbWriter.write(subclassOfFacts.toString());

        kbWriter.write("% Entities schema\n\n");
        kbWriter.write(entitiesFacts.toString() + "\n");

    }

    public static void writeRelationships(XMLSchemaScanner xml, FileWriter kbWriter) throws IOException {

        StringBuilder subclassOfFacts = new StringBuilder();
        StringBuilder relationshipsFacts = new StringBuilder();

        Iterator<Relationship> itRelationships = xml.iteratorRelationships();

        kbWriter.write("% RELATIONSHIPS\n\n");

        while (itRelationships.hasNext()) {

            Relationship originalClass = itRelationships.next();

            HashMap<String, ArrayList<String>> facts = obtainPrologFact(originalClass);

            subclassOfFacts.append(String.join("\n", facts.get("subclassOfFacts")));
            relationshipsFacts.append(String.join("\n", facts.get("relationshipFacts")));

            if (facts.get("inverseOfFacts").size() != 0) {
                relationshipsFacts.append("\n");  // separator between "normal" and "inverseOf" predicates
                relationshipsFacts.append(String.join("\n", facts.get("inverseOfFacts")));
            }

            // double visual separator for different taxonomies
            if (facts.get("subclassOfFacts").size() != 0) {
                subclassOfFacts.append("\n\n");
            }
            relationshipsFacts.append("\n\n");

        }

        kbWriter.write("% RELATIONSHIPS\n\n");

        kbWriter.write("% Relationships hierarchy\n\n");
        kbWriter.write(subclassOfFacts.toString());

        kbWriter.write("% Relationships schema\n\n");
        kbWriter.write(relationshipsFacts.toString() + "\n");
    }

    public static void writeRules(FileWriter kbWriter) throws IOException {

        kbWriter.write("% GENERIC RULES\n\n");

        // ################ Write is_subclass predicate ################
        String isSubclass1 =
                """
                is_subclass(Subclass, Superclass) :-
                \tsubclass_of(Subclass, Superclass),
                \t\\=(Subclass, Superclass).""";

        String isSubclass2 =
                """
                is_subclass(Subclass, Superclass) :-
                \tsubclass_of(Subclass, Middleclass),
                \tis_subclass(Middleclass, Superclass).
                """;

        kbWriter.write("%s\n%s\n\n".formatted(isSubclass1, isSubclass2));


        // ################ write invert_subj_obj predicate ################
        String baseInvertSubjObj = "invert_subj_obj([], []).";
        String stepinvertSubjObj =
                """
                invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
                \tinvert_subj_obj(T1, T2).""";

        String invertSubjObj = String.format("%s\n%s", baseInvertSubjObj, stepinvertSubjObj);

        // ################ write invert_relationship predicate ################
        String invertRelationship =
                """
                invert_relationship(RelationshipName, InvertedRelationship) :-
                \tRelationshipToInvert =.. [RelationshipName, SubjObjList, AttributeList],
                \tcall(RelationshipToInvert),
                \tinvert_subj_obj(SubjObjList, InvertedSubjObjList),
                \tinverse_of(InvertedRelationshipName, RelationshipName),
                \tInvertedRelationship =.. [InvertedRelationshipName, InvertedSubjObjList, AttributeList].
                """;

        kbWriter.write(String.format("%s\n\n%s\n\n", invertSubjObj, invertRelationship));


        // ################ write gather_parent_attributes predicate ################
        String baseGatherParentAttributes = "gather_parent_attributes([], []).";
        String stepGatherParentAttributes =
                """
                gather_parent_attributes([ParentC|ParentClasses], [ParentCAttributes|OtherParentAttributes]) :-
                \tClause =.. [ParentC, ParentCAttributes],
                \tcall(Clause),
                \tgather_parent_attributes(ParentClasses, OtherParentAttributes).""";

        kbWriter.write(String.format("%s\n%s\n\n", baseGatherParentAttributes, stepGatherParentAttributes));

        // ################ write gather_attributes predicate ################
        String gatherAttributes = """
                gather_attributes(SubC, Attributes) :-
                \tfindall(SuperC, is_subclass(SubC, SuperC), L),
                \tgather_parent_attributes(L, ParentAttributes),
                \tClause =.. [SubC, SubCAttributes],
                \tcall(Clause),
                \tflatten([ParentAttributes|SubCAttributes], Attributes).""";

        kbWriter.write(gatherAttributes + "\n");

    }

    // {'superclassFacts': ['subClassOf()', ...], 'facts': []}

    public static HashMap<String, ArrayList<String>> obtainPrologFact(Entity xmlEntity) {

        HashMap<String, ArrayList<String>> facts = new HashMap<>();
        facts.put("entityFacts", new ArrayList<>());
        facts.put("subclassOfFacts", new ArrayList<>());

        for (Entity subClass : xmlEntity.taxonomy) {

            HashMap<String, ArrayList<String>> taxonomyFacts = obtainPrologFact(subClass);
            facts.get("entityFacts").addAll(taxonomyFacts.get("entityFacts"));
            facts.get("subclassOfFacts").addAll(taxonomyFacts.get("subclassOfFacts"));
        }

        // wrap name with quotes to respect format of class name (mantain "/", first upper letter, ...)
        String predicateName = "'%s'".formatted(xmlEntity.name);

        // extracting attributes name
        ArrayList<String> attributeList = new ArrayList<>();

        for (HashMap<String, Object> attribute : xmlEntity.attributes) {
            attributeList.add((String) attribute.get("name"));

        }

        // format of the fact is: PredicateName(AttributesList, ParentsList)
        String firstArgument = "[%s]".formatted(String.join(", ", attributeList));

        String originalFact = "%s(%s).".formatted(predicateName, firstArgument);
        facts.get("entityFacts").add(0, originalFact);

        if (xmlEntity.parent != null) {
            facts.get("subclassOfFacts").add(0, "subclass_of('%s', '%s').".formatted(xmlEntity.name, xmlEntity.parent.name));
        }

        return facts;
    }

    public static HashMap<String, ArrayList<String>> obtainPrologFact(Relationship xmlRelationship) {

        HashMap<String, ArrayList<String>> facts = new HashMap<>();
        facts.put("relationshipFacts", new ArrayList<>());
        facts.put("inverseOfFacts", new ArrayList<>());
        facts.put("subclassOfFacts", new ArrayList<>());

        for (Relationship subClass : xmlRelationship.taxonomy) {
            HashMap<String, ArrayList<String>> taxonomyFacts = obtainPrologFact(subClass);
            facts.get("relationshipFacts").addAll(taxonomyFacts.get("relationshipFacts"));
            facts.get("inverseOfFacts").addAll(taxonomyFacts.get("inverseOfFacts"));
            facts.get("subclassOfFacts").addAll(taxonomyFacts.get("subclassOfFacts"));
        }

        // wrap name with quotes to respect format of class name (mantain "/", first upper letter, ...)
        String predicateName = "'%s'".formatted(xmlRelationship.name);

        // extracting attributes name
        ArrayList<String> subjObjList = new ArrayList<>();

        for (HashMap<String, String> reference : xmlRelationship.references) {
            subjObjList.add("'%s'-'%s'".formatted(reference.get("subject"), reference.get("object")));
        }

        // extracting attributes name
        ArrayList<String> attributeList = new ArrayList<>();

        for (HashMap<String, Object> attribute : xmlRelationship.attributes) {
            attributeList.add((String) attribute.get("name"));

        }

        // format of the fact is: PredicateName(SubjObjList, AttributeList)
        String firstArgument = "[%s]".formatted(String.join(", ", subjObjList));
        String secondArgument = "[%s]".formatted(String.join(", ", attributeList));

        String originalFact = "%s(%s, %s).".formatted(predicateName, firstArgument, secondArgument);
        facts.get("relationshipFacts").add(0, originalFact);

        if (xmlRelationship.inverseRelationship != null) {
            facts.get("inverseOfFacts").add(0, "inverse_of('%s', '%s').".formatted(xmlRelationship.inverseRelationship.name, xmlRelationship.name));
        }

        if (xmlRelationship.parent != null) {
            facts.get("subclassOfFacts").add(0, "subclass_of('%s', '%s').".formatted(xmlRelationship.name, xmlRelationship.parent.name));
        }

        return facts;
    }

}
