package src_java.xmlschemaelement;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.HashMap;


public abstract class XMLSchemaElement {

    public String name;
    public ArrayList<HashMap<String, Object>> attributes;
    protected HashMap<String, NodeList> itemContentTree;

    protected XMLSchemaElement(Node itemNode) {

        this.name = gatherName(itemNode);
        this.itemContentTree = gatherContent(itemNode);

        this.attributes = extractAttributes(itemContentTree.get("attributes"));

    }

    protected XMLSchemaElement(String name, ArrayList<HashMap<String, Object>> attributes) {

        this.name = name;
        this.attributes = attributes;

    }

    protected String gatherName(Node itemNode) {

        return itemNode.getAttributes().getNamedItem("name").getNodeValue();
    }

    protected abstract HashMap<String, NodeList> gatherContent(Node itemNode);

    protected ArrayList<HashMap<String, Object>> extractAttributes(NodeList attributesNodeList) {

        ArrayList<HashMap<String, Object>> attributes = new ArrayList<>();

        for (int i = 0; i < attributesNodeList.getLength(); i++) {
            Node node = attributesNodeList.item(i);
            if (node.getNodeType() != Node.TEXT_NODE) {

                NamedNodeMap nodeAttributes = node.getAttributes();

                HashMap<String, Object> attribute = new HashMap<>();

                for (int j = 0; j < nodeAttributes.getLength(); j++) {

                    String key = nodeAttributes.item(j).getNodeName();
                    String value = nodeAttributes.item(j).getNodeValue();

                    attribute.put(key, value);
                }

                if (attribute.get("datatype").equals("select")) {

                    ArrayList<String> possibleValues = extractPossibleValues(node);
                    attribute.put("values", possibleValues);
                }

                attributes.add(attribute);
            }
        }

        return attributes;
    }

    private ArrayList<String> extractPossibleValues(Node attributeNode) {

        ArrayList<String> possibleValues = new ArrayList<>();

        // we can't have hierarchy of attributes, so this is enough w/o cycling
        // all child nodes
        NodeList possibleValuesNodes = ((Element) attributeNode).getElementsByTagName("value");

        for (int i = 0; i < possibleValuesNodes.getLength(); i++) {

            Node singleValue = possibleValuesNodes.item(i);
            String val = singleValue.getAttributes().getNamedItem("name").getNodeValue();

            possibleValues.add(val);

        }

        return possibleValues;
    }
}
