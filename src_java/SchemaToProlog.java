package src_java;

import src_java.xmlschemaelement.Entity;
import src_java.xmlschemaelement.Relationship;
import src_java.xmlschemaelement.XMLSchemaScanner;

import java.io.File;
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

        outputFilenameKb = "data/processed/%s.pl".formatted(new File(outputFilenameKb).getName());
        
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
        kbWriter.write(":- discontiguous inverse_of/2.\n");
        kbWriter.write(":- discontiguous subclass_of/2.\n");
        kbWriter.write("% Uncomment this if you're using SWI\n% :- set_prolog_flag(unknown, fail).\n\n\n");

    }

    public static void writeEntities(XMLSchemaScanner xml, FileWriter kbWriter) throws IOException {

        StringBuilder subclassOfFacts = new StringBuilder();
        StringBuilder entitiesFacts = new StringBuilder();

        Iterator<Entity> itEntities = xml.iteratorEntities();
        while (itEntities.hasNext()) {

            Entity originalClass = itEntities.next();

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

        kbWriter.write(KBStaticRules.obtainIsSubclass() + "\n\n\n");
        kbWriter.write(KBStaticRules.obtainInvertRelationship() + "\n\n\n");
        kbWriter.write(KBStaticRules.obtainGatherAttributes() + "\n\n\n");
        kbWriter.write(KBStaticRules.obtainGatherReferences() + "\n");

    }

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


class KBStaticRules {


    private KBStaticRules() {
        throw new AssertionError("Utility class!");
    }

    public static String obtainIsSubclass() {
        // ################ Write is_subclass_normal predicate ################
        String isSubclassNormal =
                """
                is_subclass_normal(Subclass, Superclass) :-
                \tsubclass_of(Subclass, Superclass).
                
                is_subclass_normal(Subclass, Superclass) :-
                \tsubclass_of(Subclass, Middleclass),
                \tis_subclass_normal(Middleclass, Superclass).
                
                
                """;

        // ################ Write is_subclass_normal predicate ################
        String isSubclassInverse =
                """
                is_subclass_inverse(SubclassInverse, SuperclassInverse) :-
                \tinvert_relationship(SubclassInverse, SubclassClause),
                \tSubclassClause =.. [SubclassName|_],
                \tis_subclass_normal(SubclassName, SuperclassName),
                \tinvert_relationship(SuperclassName, SuperclassClauseInverse),
                \tSuperclassClauseInverse =.. [SuperclassInverse|_].
                
                
                """;

        // ################ Write is_subclass_normal predicate ################
        String isSubclass =
                """
                is_subclass(Subclass, Superclass) :-
                \tis_subclass_normal(Subclass, Superclass).
                
                is_subclass(Subclass, Superclass) :-
                \t\\+ is_subclass_normal(Subclass, _),
                \tis_subclass_inverse(Subclass, Superclass).""";

        String intro = "% ------------ is_subclass ------------\n\n";

        return intro + isSubclassNormal + isSubclassInverse + isSubclass;
    }

    public static String obtainInvertRelationship() {

        // ################ write invert_subj_obj predicate ################
        String baseInvertSubjObj = "invert_subj_obj([], []).\n\n";
        String stepinvertSubjObj =
                """
                invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
                \tinvert_subj_obj(T1, T2).
                
                
                """;

        String invertSubjObj = baseInvertSubjObj + stepinvertSubjObj;

        String invertRelationshipNormal =
                """
                % we are inverting a "normal" relationship
                invert_relationship(RelationshipName, InvertedRelationship) :-
                \tinverse_of(InvertedRelationshipName, RelationshipName),
                \t!,
                \tRelationshipToInvert =.. [RelationshipName, SubjObjList, AttributeList],
                \tcall(RelationshipToInvert),
                \tinvert_subj_obj(SubjObjList, InvertedSubjObjList),
                \tInvertedRelationship =.. [InvertedRelationshipName, InvertedSubjObjList, AttributeList].
                
                """;

        String invertRelationshipInverted =
                """
                % we are inverting an "inverted" relationship
                invert_relationship(InvertedRelationshipName, Relationship) :-
                \tinverse_of(InvertedRelationshipName, RelationshipName),
                \tRelationship =.. [RelationshipName, _SubjObjList, _Attributes],
                \tcall(Relationship),
                \t!.
                 
                """;

        // ################ write invert_relationship predicate ################
        String invertRelationshipReflexive =
                """
                % we are inverting a relationship which name=inverse
                invert_relationship(RelationshipName, Relationship) :-
                \tRelationship =.. [RelationshipName, _SubjObjList, _AttributeList],
                \tcall(Relationship).""";

        String invertRelationship = invertRelationshipNormal + invertRelationshipInverted + invertRelationshipReflexive;


        String intro = "% ------------ invert_relationship ------------\n\n";

        return intro + invertSubjObj + invertRelationship;

    }

    public static String obtainGatherAttributes() {

        // ################ write gather_parent_attributes predicate ################
        String baseGatherParentAttributes = "gather_parent_attributes([], []).\n\n";
        String stepGatherParentAttributesEntities =
                """
                % for entities
                gather_parent_attributes([ImmediateParentC|ParentClasses], [ImmediateParentCAttributes|ParentAttributes]) :-
                \tClause =.. [ImmediateParentC, ImmediateParentCAttributes],
                \tcall(Clause),
                \t!,
                \tgather_parent_attributes(ParentClasses, ParentAttributes).
                
                """;

        String stepGatherParentAttributesRelationships =
                """
                % for relationships
                gather_parent_attributes([ImmediateParentC|ParentClasses], [ImmediateParentCAttributes|ParentAttributes]) :-
                \tClause =.. [ImmediateParentC, _ParentCReferences, ImmediateParentCAttributes],
                \tcall(Clause),
                \tgather_parent_attributes(ParentClasses, ParentAttributes).
                
                
                """;

        String gatherParentAttributes = baseGatherParentAttributes +
                                        stepGatherParentAttributesEntities +
                                        stepGatherParentAttributesRelationships;

        // ################ write gather_attributes predicate ################
        String gatherAttributesEntities =
                """
                % for entities
                gather_attributes(SubC, Attributes) :-
                \tClause =.. [SubC, SubCAttributes],
                \tcall(Clause),
                \t!,
                \tfindall(SuperC, is_subclass(SubC, SuperC), ParentsList),
                \tgather_parent_attributes(ParentsList, ParentAttributes),
                \tflatten([ParentAttributes|SubCAttributes], Attributes).
                
                """;

        String gatherAttributesRelationshipsNormal =
                """
                % for relationships
                gather_attributes(SubC, Attributes) :-
                \tClause =.. [SubC, _SubCReferences, SubCAttributes],
                \tcall(Clause),
                \t!, % it means we are processing a normal relationship (i.e. not inversed)
                \tfindall(SuperC, is_subclass(SubC, SuperC), ParentList),
                \tgather_parent_attributes(ParentList, ParentAttributes),
                \tflatten([ParentAttributes|SubCAttributes], Attributes).
                
                """;

        String gatherAttributesRelationshipsInverted =
                """
                gather_attributes(InverseSubC, Attributes) :-
                \tinverse_of(InverseSubC, SubC),
                \tgather_attributes(SubC, Attributes).""";

        String gatherAttributesRelationships =
                gatherAttributesRelationshipsNormal + gatherAttributesRelationshipsInverted;

        String gatherAttributes = gatherAttributesEntities + gatherAttributesRelationships;

        String intro = "% ------------ gather_attributes ------------\n\n";

        return intro + gatherParentAttributes + gatherAttributes;
    }

    public static String obtainGatherReferences() {

        // ################ write gather_parent_references predicate ################
        String baseGatherParentReferences = "gather_parent_references([], []).\n\n";
        String stepGatherParentReferences =
                """
                gather_parent_references([ImmediateParentC|ParentClasses], [ImmediateParentCReferences|ParentCReferences]) :-
                \tClause =.. [ImmediateParentC, ImmediateParentCReferences, _],
                \tcall(Clause),
                \tgather_parent_references(ParentClasses, ParentCReferences).
                
                
                """;

        String gatherParentReferences = baseGatherParentReferences + stepGatherParentReferences;

        // ################ write gather_references predicate ################
        String gatherReferencesNormal =
                """
                gather_references(SubC, References) :-
                \tClause =.. [SubC, SubCReferences, _SubCAttributes],
                \tcall(Clause),
                \t!, % it means we are processing a normal relationship (i.e. not inversed)
                \tfindall(SuperC, is_subclass(SubC, SuperC), ParentsList),
                \tgather_parent_references(ParentsList, ParentReferences),
                \tflatten([ParentReferences|SubCReferences], References).
                
                """;

        String gatherReferencesInverted =
                """
                gather_references(InverseSubC, InvertedReferences) :-
                \tinverse_of(InverseSubC, SubC),
                \tgather_references(SubC, References),
                \tinvert_subj_obj(References, InvertedReferences).""";

        String gatherReferences = gatherReferencesNormal + gatherReferencesInverted;

        String intro = "% ------------ gather_references ------------\n\n";

        return intro + gatherParentReferences + gatherReferences;
    }
}