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


public class XMLSchemaScanner implements Iterable<XMLSchemaElement> {

    private NodeList entities;
    private NodeList relationships;

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

    @Override
    public Iterator<XMLSchemaElement> iterator() {
        return new Iterator<>() {

            private int current = 0;
            private final int max = entities.getLength() + relationships.getLength();

            @Override
            public boolean hasNext() {
                return current < max;
            }

            @Override
            public XMLSchemaElement next() {

                XMLSchemaElement item;

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
}
