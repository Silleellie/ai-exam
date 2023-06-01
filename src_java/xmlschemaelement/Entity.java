package src_java.xmlschemaelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.HashMap;


/**
 * Class representing the Entity element of the GraphBrain XML Schema
 */
public class Entity extends XMLSchemaElement<Entity> {

    /**
     * Constructor to be called when instantiating any top level Entity
     *
     * @param node The XML Node pointing to the GraphBrain top class entity to process
     */
    protected Entity(Node node) {

        super(node);
        this.taxonomy = extractTaxonomy(itemContentTree.get("taxonomy"));
    }

    /**
     * Constructor to be called when instantiating any subclass of an entity taxonomy
     *
     * @param itemNode The XML Node pointing to the GraphBrain subclass entity to process
     * @param parentEntity The immediate parent entity of the currently processed GraphBrain subclass
     */
    protected Entity(Node itemNode, Entity parentEntity) {

        super(itemNode, parentEntity);
        this.taxonomy = extractTaxonomy(itemContentTree.get("taxonomy"));

    }

    /**
     * Gather all the XML content tree related to the entity to instantiate.
     *
     * @param currentNode The current "pointer" in the XML file
     * @return The hashmap having as keys the XML tags and as values their nodes
     */
    protected HashMap<String, NodeList> gatherContent(Node currentNode) {

        HashMap<String, NodeList> entityContent = new HashMap<>();

        NodeList childNodes = currentNode.getChildNodes();

        // simple trick to initialize empty NodeList with a non-existent tag
        NodeList attributeList = ((Element) currentNode).getElementsByTagName("<empty>");
        NodeList taxonomyList = ((Element) currentNode).getElementsByTagName("<empty>");

        for (int i = 0; i < currentNode.getChildNodes().getLength(); i++) {

            Node child = childNodes.item(i);
            if (child instanceof Element childElement) {

                switch (childElement.getTagName()) {
                    case "attributes" -> attributeList = child.getChildNodes();
                    case "taxonomy" -> taxonomyList = child.getChildNodes();
                }
            }

        }

        entityContent.put("attributes", attributeList);
        entityContent.put("taxonomy", taxonomyList);

        return entityContent;
    }

    /**
     * Method which extracts all subclasses in the taxonomy of the entity to instantiate
     *
     * @param taxonomiesNodeList The XML Nodes in the taxonomy section of the current entity
     * @return An Array of Entity objects, one for each subclass of the current entity
     */
    protected ArrayList<Entity> extractTaxonomy(NodeList taxonomiesNodeList) {

        ArrayList<Entity> taxonomy = new ArrayList<>();

        for (int i = 0; i < taxonomiesNodeList.getLength(); i++) {
            Node node = taxonomiesNodeList.item(i);

            if (node.getNodeType() != Node.TEXT_NODE) {

                Entity subClassEntity = new Entity(node, this);
                taxonomy.add(subClassEntity);
            }
        }

        return taxonomy;
    }

    @Override
    public String toString() {
        return "Entity - %s".formatted(this.name);
    }
}
