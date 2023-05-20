package src_java.xmlschemaelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.HashMap;


public class Entity extends XMLSchemaElement<Entity> {

    protected Entity(Node node) {

        super(node);
        this.taxonomy = extractTaxonomy(itemContentTree.get("taxonomy"));
    }

    protected Entity(Node itemNode, Entity parentEntity) {

        super(itemNode, parentEntity);
        this.taxonomy = extractTaxonomy(itemContentTree.get("taxonomy"));

    }

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

}
