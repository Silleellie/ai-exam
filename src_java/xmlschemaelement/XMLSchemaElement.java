package src_java.xmlschemaelement;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Abstract class representing an element of the GraphBrain XML Schema
 * 
 * @param <T> Either Entity or Relationship subclasses
 */
public abstract class XMLSchemaElement<T extends XMLSchemaElement<T>> {

    public String name;
    public ArrayList<HashMap<String, Object>> attributes;
    public ArrayList<HashMap<String, Object>> allAttributes;
    public ArrayList<T> taxonomy;
    protected HashMap<String, NodeList> itemContentTree;
    public T parent;

    /**
     * Constructor to be called when instantiating any top level class (Entities or Relationships)
     *
     * @param itemNode The XML Node pointing to the GraphBrain top class element to process
     */
    protected XMLSchemaElement(Node itemNode) {

        this.name = gatherName(itemNode);
        this.itemContentTree = gatherContent(itemNode);

        this.attributes = extractAttributes(itemContentTree.get("attributes"));

        this.allAttributes = new ArrayList<>();
        this.allAttributes.addAll(this.attributes);
    }

    /**
     * Constructor to be called when instantiating any subclass of a taxonomy (Entities or Relationships)
     *
     * @param itemNode The XML Node pointing to the GraphBrain subclass element to process
     * @param parent The immediate parent element of the currently processed GraphBrain subclass
     */
    protected XMLSchemaElement(Node itemNode, T parent) {

        this.name = gatherName(itemNode);
        this.itemContentTree = gatherContent(itemNode);

        this.attributes = extractAttributes(itemContentTree.get("attributes"));

        this.allAttributes = new ArrayList<>();
        if (parent != null) {
            this.allAttributes.addAll(parent.allAttributes);
        }
        this.allAttributes.addAll(this.attributes);

        this.parent = parent;

    }

    /**
     * Constructor to be called without referring to an XML node, but rather using static information
     * passed as input. Useful for instantiating Inverse Relationships (which don't have an explicit Node in the
     * GraphBrain schema)
     *
     * @param name String representing the name of the GraphBrain element
     * @param attributes Hashmap where keys are name of the attributes and values are their values +
     *                   an array list of possible values if the attribute is of type 'select'
     * @param taxonomy ArrayList of all the subclasses of the element to instantiate
     * @param itemContentTree The XML content tree related to the element to instantiate (for inverse relationships this
     *                        is the same of the original relationship)
     * @param parent The immediate parent element of the currently processed GraphBrain subclass
     */
    protected XMLSchemaElement(String name,
                               ArrayList<HashMap<String, Object>> attributes,
                               ArrayList<T> taxonomy,
                               HashMap<String, NodeList> itemContentTree,
                               T parent) {

        this.name = name;
        this.attributes = attributes;
        this.taxonomy = taxonomy;
        this.itemContentTree = itemContentTree;
        this.parent = parent;

        this.allAttributes = new ArrayList<>();
        if (parent != null) {
            this.allAttributes.addAll(parent.allAttributes);
        }
        this.allAttributes.addAll(this.attributes);
    }


    /**
     * Method which extracts the name of the current GraphBrain schema element which is being processed
     *
     * @param itemNode XMLNode of the currently processed element
     * @return String representing the name of the GraphBrain element
     */
    protected String gatherName(Node itemNode) {

        return itemNode.getAttributes().getNamedItem("name").getNodeValue();
    }

    protected abstract HashMap<String, NodeList> gatherContent(Node itemNode);

    /**
     * Method which extracts local attributes of the currently processed GraphBrain element,
     * given its {@literal <attributes>} section
     *
     * @param attributesNodeList {@literal <attributes>} section of the currently processed GraphBrain element
     * @return Hashmap where keys are name of the attributes and values are their values + an array list of possible
     * values if the attribute is of type 'select'
     */
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

    /**
     * Method which extracts, in case of a 'select' attribute, all of its possible values
     *
     * @param attributeNode Currently processed attribute XML node
     * @return ArrayList containing all possible values for the attribute
     */
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

    abstract protected ArrayList<T> extractTaxonomy(NodeList taxonomiesNodeList);
}
