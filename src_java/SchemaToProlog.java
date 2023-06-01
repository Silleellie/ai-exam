package src_java;

import src_java.xmlschemaelement.Entity;
import src_java.xmlschemaelement.Relationship;
import src_java.xmlschemaelement.XMLSchemaScanner;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;


/**
 * Class responsible for converting the XML GraphBrain schema to the Prolog formalism
 */
public class SchemaToProlog {

    /**
     * It will return the XML relative filepath to convert from the 'inputs' folder.
     * If more than one XML is present, the user will be prompted to choose the appropriate one
     *
     * @param inputScanner The Scanner object reading user input from stdin
     * @return The relative path to the XML schema to con
     * @throws FileNotFoundException If no XML schema is found in the 'inputs' folder
     */
    private static String getInputFilename(Scanner inputScanner) throws FileNotFoundException {

        List<String> result;
        String inputFilename;

        try (Stream<Path> walk = Files.walk(Paths.get("inputs"))) {
            result = walk
                    .filter(p -> !Files.isDirectory(p))   // not a directory
                    .map(Path::toString)                  // convert path to string
                    .filter(f -> f.endsWith("xml"))       // check end with
                    .toList();                            // collect all matched to a List

        } catch (IOException e) {
            throw new FileNotFoundException("No XML file containing the GraphBrain schema found in 'inputs' folder!");
        }

        if (result.size() > 1) {

            System.out.printf("Found %d possible schema files! Please choose the correct one to use:%n", result.size());

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

            System.out.printf("Found one possible schema file: %s will be used%n", fileName);

        }

        return inputFilename;
    }


    /**
     * Method to write Prolog directives at the top of the converted schema
     *
     * @param schemaWriter FileWriter object to write data in the converted schema prolog file
     * @throws IOException If an I/O error occurs
     */
    private static void writeDirectives(FileWriter schemaWriter) throws IOException {

        schemaWriter.write(":- use_module(library(lists)).\n");
        schemaWriter.write(":- discontiguous inverse_of/2.\n");
        schemaWriter.write(":- discontiguous subclass_of/2.\n");
        schemaWriter.write(":- unknown(_, fail).\n\n\n");

    }

    /**
     * Method which converts and writes all the Entity definitions in the original schema.
     * <p>
     * First it iterates over all entities found in the XML file and obtains two different kinds of prolog facts for
     * each entity:
     *  - Subclass facts: to create a hierarchy of facts based on the taxonomy that is present in the XML schema (subclass_of)
     * <p>
     *     subclass_of(EntityName, EntityName).
     *     ex: subclass_of('Artwork', 'Artifact').
     * <p>
     *  - Entity facts: entity definitions (attribute names) converted from the XML schema to prolog language
     * <p>
     *    EntityName(AttributesList).
     *    ex:'Artifact'([name, description]).
     * <p>
     *  All entity facts will only have attributes related to their specific class, navigating the taxonomy is necessary
     *  to retrieve all the attributes for an entity.
     *  Once all the facts have been retrieved, they are written to the converted prolog schema file.
     *
     * @param xml XMLSchemaScanner which is used to navigate the original XML schema
     * @param schemaWriter FileWriter object to write data in the converted schema prolog file
     * @throws IOException If an I/O error occurs
     */
    private static void writeEntities(XMLSchemaScanner xml, FileWriter schemaWriter) throws IOException {

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

        schemaWriter.write("% ENTITIES\n\n");

        schemaWriter.write("% Entities hierarchy\n\n");
        schemaWriter.write(subclassOfFacts.toString());

        schemaWriter.write("% Entities schema\n\n");
        schemaWriter.write(entitiesFacts.toString() + "\n");

    }

    /**
     * Method which converts and writes all the Relationship definitions in the original schema.
     * <p>
     * First it iterates over all relationships found in the XML file and obtains three different kinds of prolog facts for
     * each relationship:
     *  - Subclass facts: to create a hierarchy of facts based on the taxonomy that is present in the XML schema (subclass_of)
     * <p>
     *    subclass_of(RelationshipName, RelationshipName).
     *    ex: subclass_of('Artwork', 'Artifact').
     * <p>
     *  - Inverted Relationship facts: to keep track of inverse relationships (inverse_of)
     * <p>
     *    inverse_of(RelationshipName, RelationshipName).
     *    ex: inverse_of('knownBy', 'knows').
     * <p>
     *  - Relationship facts: relationship definitions (attribute names and Subject-Object type pairs) converted from
     *      the XML schema to prolog language
     * <p>
     *    RelationshipName(ReferencesList, AttributesList).
     *    ex:'knows'(['Person'-'Category'], [role]).
     * <p>
     *  All relationship facts will only have attributes and Subject Object pairs related to their specific class,
     *  navigating the taxonomy is necessary to retrieve all the attributes and references for a relationship.
     *  Once all the facts have been retrieved, they are written to the converted prolog schema file.
     *
     * @param xml XMLSchemaScanner which is used to navigate the original XML schema
     * @param schemaWriter FileWriter object to write data in the converted schema prolog file
     * @throws IOException If an I/O error occurs
     */
    private static void writeRelationships(XMLSchemaScanner xml, FileWriter schemaWriter) throws IOException {

        StringBuilder subclassOfFacts = new StringBuilder();
        StringBuilder relationshipsFacts = new StringBuilder();

        Iterator<Relationship> itRelationships = xml.iteratorRelationships();

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

        schemaWriter.write("% RELATIONSHIPS\n\n");

        schemaWriter.write("% Relationships hierarchy\n\n");
        schemaWriter.write(subclassOfFacts.toString());

        schemaWriter.write("% Relationships schema\n\n");
        schemaWriter.write(relationshipsFacts.toString() + "\n");
    }

    /**
     * Method which writes generic rules for the Prolog converted schema.
     * The main rules written are:
     *   - is_subclass predicate, to check whether a class is a subclass of another (at any level)
     *   - invert_relationship predicate, to obtain the inverse relationship (if it exists)
     *   - gather_attributes predicate, to gather all attributes of a class + all attributes of its
     *     parent class (for entities and relationships)
     *   - gather_references predicate, to gather all references of a relationship + all references of its parent
     *     class
     *
     * @param schemaWriter FileWriter object to write data in the converted schema prolog file
     * @throws IOException If an I/O error occurs
     */
    private static void writeRules(FileWriter schemaWriter) throws IOException {

        schemaWriter.write("% GENERIC RULES\n\n");

        schemaWriter.write(KBStaticRules.obtainIsSubclass() + "\n\n\n");
        schemaWriter.write(KBStaticRules.obtainInvertRelationship() + "\n\n\n");
        schemaWriter.write(KBStaticRules.obtainGatherAttributes() + "\n\n\n");
        schemaWriter.write(KBStaticRules.obtainGatherReferences() + "\n");

    }

    /**
     * Method which obtains all prolog facts related to the current entity to write in the converted schema Prolog file.
     * This is called recursively to each possible subclass of the entity, so to have in the end all subclass_of and
     * entity facts for the xmlEntity object passed as parameter and for all levels of its taxonomy
     *
     * @param xmlEntity Entity object of a top Level entity from which all facts related to it and its possible
     *                  subclasses must be obtained
     * @return The Hashmap containing all subclass_of and entity facts for the xmlEntity object passed as parameter and
     * for all its possible subclasses
     */
    private static HashMap<String, ArrayList<String>> obtainPrologFact(Entity xmlEntity) {

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

    /**
     * Method which obtains all prolog facts related to the relationship to write in the converted schema Prolog file.
     * This is called recursively to each possible subclass of the relationship, so to have in the end all inverse_of,
     * subclass_of and relationship facts for the xmlRelationship object passed as parameter and for all levels of
     * its taxonomy
     *
     * @param xmlRelationship Relationship object of a top Level relationship from which all facts related to it and its
     *                        possible subclasses must be obtained
     * @return The Hashmap containing all inverse_of, subclass_of and relationship facts for the xmlRelationship object
     * passed as parameter and for all its possible subclasses
     */
    private static HashMap<String, ArrayList<String>> obtainPrologFact(Relationship xmlRelationship) {

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

    /**
     * Method to call to start the whole conversion from the XML schema file contained in the 'inputs' folder to
     * the Prolog file which will be saved in the 'outputs' folder.
     * If more than one eligible XML file is found, the user is prompted to choose the appropriate one
     *
     * @throws IOException If an I/O error occurs
     */
    public static void main (String[] args) throws IOException {

        Scanner input = new Scanner(System.in);

        String inputFilename = getInputFilename(input);

        // Strip extension from inputFilename and add .pl
        String inputFilenameStripped = inputFilename.substring(0, inputFilename.lastIndexOf('.'));
        String outputFilenameKb = "outputs/schema_%s.pl".formatted(new File(inputFilenameStripped).getName());

        XMLSchemaScanner xml = new XMLSchemaScanner(inputFilename);
        FileWriter schemaWriter = new FileWriter(outputFilenameKb);

        writeDirectives(schemaWriter);
        writeEntities(xml, schemaWriter);
        writeRelationships(xml, schemaWriter);
        writeRules(schemaWriter);

        schemaWriter.close();

        String outputPath = FileSystems.getDefault().getPath(outputFilenameKb).toAbsolutePath().toString();
        System.out.printf("Converted schema saved into %s!%n", outputPath);

    }
}

/**
 * Utility class which contains the generic rules implementation related to the converted XML schema in Prolog
 */
class KBStaticRules {


    private KBStaticRules() {
        throw new AssertionError("Utility class! Can't be instantiated!");
    }

    /**
     * Returns all prolog rules to check whether an entity / relationship is a subclass of another one.
     * In particular, the following cases are taken into consideration:
     * <p>
     *  - is_subclass_normal(Subclass, Superclass): True if the Entity / Relationship subclass inherits from Superclass
     *      at any hierarchy level;
     *  - is_subclass_inverse(Subclass, Superclass): True if the inverse of the Relationship subclass inherits from the
     *      inverse of the Superclass at any hierarchy level;
     *  - is_subclass(Subclass, Superclass): checks for both cases described above.
     */
    public static String obtainIsSubclass() {

        // ################ Write is_subclass predicate ################
        String isSubclass =
                """
                %% is_subclass(?Subclass, ?Superclass)
                %
                % Predicate to check if Subclass is a subclass at any hierarchy level of Superclass.
                % This will work for any GraphBrain type, let it be entities, relationships or inverse relationships
                
                is_subclass(Subclass, Superclass) :-
                \tis_subclass_normal(Subclass, Superclass).
                
                is_subclass(Subclass, Superclass) :-
                \t\\+ is_subclass_normal(Subclass, _),
                \tis_subclass_inverse(Subclass, Superclass).
                
                
                """;

        // ################ Write is_subclass_normal predicate ################
        String isSubclassNormal =
                """
                % is_subclass_normal(?Subclass, ?Superclass)
                %
                % Predicate to check if Subclass is a subclass at any hierarchy level of Superclass.
                % This won't work for inverse relationships
                
                is_subclass_normal(Subclass, Superclass) :-
                \tsubclass_of(Subclass, Superclass).
                
                is_subclass_normal(Subclass, Superclass) :-
                \tsubclass_of(Subclass, Middleclass),
                \tis_subclass_normal(Middleclass, Superclass).
                
                
                """;

        // ################ Write is_subclass_inverse predicate ################
        String isSubclassInverse =
                """
                % is_subclass_inverse(?SubclassInverse, ?SuperclassInverse)
                %
                % Predicate to check if SubclassInverse is a subclass at any hierarchy level of SuperclassInverse.
                % This will ONLY work for inverse relationships, and it is typically used to retrieve all 
                % parents/children of an inverse relationship
                
                is_subclass_inverse(SubclassInverse, SuperclassInverse) :-
                \tinvert_relationship(SubclassInverse, SubclassClause),
                \tSubclassClause =.. [SubclassName|_],
                \tis_subclass_normal(SubclassName, SuperclassName),
                \tinvert_relationship(SuperclassName, SuperclassClauseInverse),
                \tSuperclassClauseInverse =.. [SuperclassInverse|_].""";

        return isSubclass + isSubclassNormal + isSubclassInverse;
    }

    /**
     * Returns all prolog rules to obtain the inverse of a relationship.
     * The basic rule to invert references (Subject-Object pairs to Object-Subject pairs) invert_subj_obj is provided.
     * Then, the following cases are taken into consideration:
     * N.B.: all predicates return the inverted relationship clause itself and not just its name.
     * <p>
     *  - invert_relationship(RelationshipName, InvertedRelationship): True if inverseOf(InvertedRelationshipName, RelationshipName)
     *      where InvertedRelationshipName is the predicate name of InvertedRelationship.
     *  - invert_relationship(InvertedRelationshipName, Relationship): True if inverseOf(InvertedRelationshipName, RelationshipName)
     *      where RelationshipName is the predicate name of Relationship.
     *  - invert_relationship(RelationshipName, Relationship): True if the relationship doesn't have an inverse (therefore the inverse is itself)
     */
    public static String obtainInvertRelationship() {

        // ################ write invert_relationship predicate ################
        String invertRelationshipNormal =
                """
                %% invert_relationship(+RelationshipName, -InvertedRelationshipClause)
                %
                % Predicate which, given a RelationshipName returns its inverse relationship clause.
                % This predicate is reflexive (i.e. given an inverted relationship name, the output will be the original
                % relationship clause).
                % If the relationship has no inverse, the output clause is the clause of the input relationship itself.
                
                % we are inverting a "normal" relationship
                invert_relationship(RelationshipName, InvertedRelationshipClause) :-
                \tinverse_of(InvertedRelationshipName, RelationshipName),
                \t!,
                \tRelationshipToInvert =.. [RelationshipName, SubjObjList, AttributeList],
                \tcall(RelationshipToInvert),
                \tinvert_subj_obj(SubjObjList, InvertedSubjObjList),
                \tInvertedRelationshipClause =.. [InvertedRelationshipName, InvertedSubjObjList, AttributeList].
                
                """;

        String invertRelationshipInverted =
                """
                % we are inverting an "inverted" relationship
                invert_relationship(InvertedRelationshipName, RelationshipClause) :-
                \tinverse_of(InvertedRelationshipName, RelationshipName),
                \tRelationshipClause =.. [RelationshipName, _SubjObjList, _Attributes],
                \tcall(RelationshipClause),
                \t!.
                 
                """;

        String invertRelationshipReflexive =
                """
                % we are inverting a relationship with no inverse
                invert_relationship(RelationshipName, Relationship) :-
                \tRelationship =.. [RelationshipName, _SubjObjList, _AttributeList],
                \tcall(Relationship).
                
                
                """;

        // ################ write invert_subj_obj predicate ################
        String invertSubjObj =
                """
                % invert_subj_obj(+SubjectObjectList, -ObjectSubjectList)
                %
                % Predicate which inverts all references of a relationship
                                
                invert_subj_obj([], []).
                                
                invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
                \tinvert_subj_obj(T1, T2).""";


        String invertRelationship = invertRelationshipNormal + invertRelationshipInverted + invertRelationshipReflexive;

        return invertRelationship + invertSubjObj;

    }

    /**
     * Returns all attributes for an Entity / Relationship from the ones in their superclasses hierarchy.
     * The rules to retrieve the parent attributes of an entity or relationship are provided:
     *  - gather_parent_attributes considers both cases
     * Then the rules to retrieve all of their attributes are also provided:
     * <p>
     *  - gather_attributes considers three possible cases:
     *      -- retrieve attributes of an entity from itself and all of its superclasses;
     *      -- retrieve attributes of a relationship from itself and all of its superclasses;
     *      -- retrieve attributes of an inverted relationship from itself and the inverse of all of its superclasses.
     */
    public static String obtainGatherAttributes() {

        // ################ write gather_attributes predicate ################
        String gatherAttributesEntities =
                """
                %% gather_attributes(+SubC, -Attributes)
                %
                % Predicate which will gather all attributes of SubC + all attributes of its parents classes in its
                % taxonomy. The order of the attributes is based on the "distance" of the parent class from SubC in its
                % hierarchy: the attributes of the furthest parent class will be shown first.
                
                % for entities
                gather_attributes(SubC, Attributes) :-
                \tClause =.. [SubC, SubCAttributes],
                \tcall(Clause),
                \t!,
                \tfindall(SuperC, is_subclass(SubC, SuperC), ParentsList),
                \tgather_parent_attributes(ParentsList, ParentAttributes),
                \treverse(ParentAttributes, ReversedParentAttributes),
                \tflatten([SubCAttributes|ReversedParentAttributes], Attributes).
                
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
                \treverse(ParentAttributes, ReversedParentAttributes),
                \tflatten([SubCAttributes|ReversedParentAttributes], Attributes).
                                
                """;

        String gatherAttributesRelationshipsInverted =
                """
                gather_attributes(InverseSubC, Attributes) :-
                \tinverse_of(InverseSubC, SubC),
                \tgather_attributes(SubC, Attributes).
                
                
                """;

        String gatherAttributesRelationships =
                gatherAttributesRelationshipsNormal + gatherAttributesRelationshipsInverted;

        String gatherAttributes = gatherAttributesEntities + gatherAttributesRelationships;

        // ################ write gather_parent_attributes predicate ################
        String baseGatherParentAttributes =
                """
                % gather_parent_attributes(+ParentClasses, -ParentAttributes)
                %
                % Predicate which will gather, for each class in the ParentClasses argument, all of their attributes.
                % The order of the attributes depends on the ParentClasses ordering
                
                gather_parent_attributes([], []).
                
                """;

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
                \tgather_parent_attributes(ParentClasses, ParentAttributes).""";

        String gatherParentAttributes = baseGatherParentAttributes +
                stepGatherParentAttributesEntities +
                stepGatherParentAttributesRelationships;

        return gatherAttributes + gatherParentAttributes;
    }

    /**
     * Returns all references for a Relationship from the ones in its superclasses hierarchy.
     * The rule to retrieve the parent references of a relationship is provided:
     *  - gather_parent_references
     * Then the rules to retrieve all of their references are also provided:
     * <p>
     *  - gather_references considers two possible cases:
     *      -- retrieve references of a relationship from itself and all of its superclasses;
     *      -- retrieve references of an inverted relationship from itself and the inverse of all of its superclasses.
     */
    public static String obtainGatherReferences() {

        // ################ write gather_references predicate ################
        String gatherReferencesNormal =
                """
                %% gather_references(+SubC, -References)
                %
                % Predicate which will gather all references of SubC + all references of its parents classes in its
                % taxonomy. The order of the references is based on the "distance" of the parent class from SubC in its
                % hierarchy: the references of the furthest parent class will be shown first.
                
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
                \tinvert_subj_obj(References, InvertedReferences).
                
                
                """;

        String gatherReferences = gatherReferencesNormal + gatherReferencesInverted;

        // ################ write gather_parent_references predicate ################
        String baseGatherParentReferences =
                """
                % gather_parent_reference(+ParentClasses, -ParentReferences)
                %
                % Predicate which will gather, for each class in the ParentClasses argument, all of their references.
                % The order of the references depends on the ParentClasses ordering
                
                gather_parent_references([], []).
                
                """;
        String stepGatherParentReferences =
                """
                gather_parent_references([ImmediateParentC|ParentClasses], [ImmediateParentCReferences|ParentCReferences]) :-
                \tClause =.. [ImmediateParentC, ImmediateParentCReferences, _],
                \tcall(Clause),
                \tgather_parent_references(ParentClasses, ParentCReferences).""";

        String gatherParentReferences = baseGatherParentReferences + stepGatherParentReferences;

        return gatherReferences + gatherParentReferences;
    }
}