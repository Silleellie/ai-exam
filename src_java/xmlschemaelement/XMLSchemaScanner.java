package src_java.xmlschemaelement;

import org.w3c.dom.NodeList;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.util.*;


/**
 * Class which implements several iterators over schema elements (entity, relationship or both)
 */
public class XMLSchemaScanner implements Iterable<XMLSchemaElement<?>> {

    private NodeList entities;
    private NodeList relationships;

    /**
     * Returns Scanner over a specified XML file containing the schema for an ontology in GraphBrain
     * @param xmlFilePath Path where the XML file is located
     */
    public XMLSchemaScanner(String xmlFilePath) {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

        try {

            DocumentBuilder db = dbf.newDocumentBuilder();
            Document document = db.parse(xmlFilePath);

            entities = document.getElementsByTagName("entity");
            relationships = document.getElementsByTagName("relationship");

        } catch (ParserConfigurationException | SAXException | IOException err) {
            System.out.println("ERROR");
            System.out.println(err.getMessage());
        }

    }

    /**
     * Main iterator method which allows to iterate over all elements of a GraphBrain ontology XML file.
     * The iterator works by first cycling over all the entities and then over all relationships.
     *
     * @return Iterator over XMLSchemaElement which returns objects of type Entity if there are still entities in the
     *  XML schema file, Relationship otherwise
     */
    @Override
    public Iterator<XMLSchemaElement<?>> iterator() {
        return new Iterator<>() {

            private int current = 0;
            private final int max = entities.getLength() + relationships.getLength();

            @Override
            public boolean hasNext() {
                return current < max;
            }

            @Override
            public XMLSchemaElement<?> next() {

                if (!hasNext()) {
                    throw new NoSuchElementException();
                }

                XMLSchemaElement<?> item;

                if (current < entities.getLength()) {
                    Node entityNode = entities.item(current);
                    item = new Entity(entityNode);
                } else {
                    Node relationshipNode = relationships.item(current - entities.getLength());
                    item = new Relationship(relationshipNode);
                }

                current++;

                return item;
            }
        };
    }

    /**
     * Iterator over all entities of the GraphBrain schema
     * 
     * @return iterator over entities in the XML schema file
     */
    public Iterator<Entity> iteratorEntities() {
        return new Iterator<>() {

            private int current = 0;
            private final int max = entities.getLength();

            @Override
            public boolean hasNext() {
                return current < max;
            }

            @Override
            public Entity next() {

                if (!hasNext()) {
                    throw new NoSuchElementException();
                }

                Node entityNode = entities.item(current);
                Entity entity = new Entity(entityNode);

                current++;

                return entity;
            }
        };
    }

    /**
     * Iterator over all relationships of the GraphBrain schema
     * 
     * @return iterator over relationships in the XML schema file
     */
    public Iterator<Relationship> iteratorRelationships() {
        return new Iterator<>() {

            private int current = 0;
            private final int max = relationships.getLength();

            @Override
            public boolean hasNext() {
                return current < max;
            }

            @Override
            public Relationship next() {

                if (!hasNext()) {
                    throw new NoSuchElementException();
                }

                Node entityNode = relationships.item(current);
                Relationship relationship = new Relationship(entityNode);

                current++;

                return relationship;
            }
        };
    }
}
