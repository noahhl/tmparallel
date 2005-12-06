/** 
 * Purpose: 	Generate a split for the Reuters21578 data set
 * Usage:	genSplit <xml-file> <xpath-expr>
 * Author: 	Ingo Feinerer
 * Origin:      xpath2.c libxml2 example from Aleksey Sanin and Daniel Veillard
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

#if defined(LIBXML_XPATH_ENABLED) && defined(LIBXML_SAX1_ENABLED) && \
    defined(LIBXML_OUTPUT_ENABLED)

static void usage(const char *name);
static int evalXPathExpr(const char *filename, const xmlChar * xpathExpr);
static void delete_xpath_nodes(xmlNodeSetPtr nodes);

int 
main(int argc, char **argv) {
    /* Parse command line and process file */
    if (argc != 3) {
	fprintf(stderr, "Error: wrong number of arguments.\n");
	usage(argv[0]);
	return(-1);
    } 
    
    /* Init libxml */     
    xmlInitParser();
    LIBXML_TEST_VERSION

    /* Do the main job */
    if (evalXPathExpr(argv[1], BAD_CAST argv[2])) {
	usage(argv[0]);
	return(-1);
    }

    /* Shutdown libxml */
    xmlCleanupParser();
    
    /*
     * this is to debug memory for regression tests
     */
    xmlMemoryDump();
    return 0;
}

/**
 * usage:
 * @name:		the program name.
 *
 * Prints usage information.
 */
static void 
usage(const char *name) {
    assert(name);
    
    fprintf(stderr, "Usage: %s <xml-file> <xpath-expr>\n", name);
}

/**
 * evalXPathExpr:
 * @filename:		the input XML filename.
 * @xpathExpr:		the xpath expression for evaluation.
 *
 * Parses input XML file, evaluates XPath expression and delete the nodes
 * then print the result.
 *
 * Returns 0 on success and a negative value otherwise.
 */
static int 
evalXPathExpr(const char* filename, const xmlChar* xpathExpr) {
    xmlDocPtr doc;
    xmlXPathContextPtr xpathCtx; 
    xmlXPathObjectPtr xpathObj; 
    
    assert(filename);
    assert(xpathExpr);

    /* Load XML document */
    doc = xmlParseFile(filename);
    if (doc == NULL) {
	fprintf(stderr, "Error: unable to parse file \"%s\"\n", filename);
	return(-1);
    }

    /* Create xpath evaluation context */
    xpathCtx = xmlXPathNewContext(doc);
    if(xpathCtx == NULL) {
        fprintf(stderr,"Error: unable to create new XPath context\n");
        xmlFreeDoc(doc); 
        return(-1);
    }
    
    /* Evaluate xpath expression */
    xpathObj = xmlXPathEvalExpression(xpathExpr, xpathCtx);
    if(xpathObj == NULL) {
        fprintf(stderr,"Error: unable to evaluate xpath expression \"%s\"\n", xpathExpr);
        xmlXPathFreeContext(xpathCtx); 
        xmlFreeDoc(doc); 
        return(-1);
    }

    /* Delete selected nodes */
    delete_xpath_nodes(xpathObj->nodesetval);
    
    /* Cleanup of XPath data */
    xmlXPathFreeObject(xpathObj);
    xmlXPathFreeContext(xpathCtx); 

    /* dump the resulting document */
    xmlDocDump(stdout, doc);


    /* free the document */
    xmlFreeDoc(doc); 
    
    return(0);
}

/**
 * delete_xpath_nodes:
 * @nodes:		the nodes set.
 */
static void
delete_xpath_nodes(xmlNodeSetPtr nodes) {
    int size;
    int i;

    size = (nodes) ? nodes->nodeNr : 0;

    for(i = size - 1; i >= 0; i--) {
	assert(nodes->nodeTab[i]);

	xmlUnlinkNode(nodes->nodeTab[i]);
	xmlFreeNode(nodes->nodeTab[i]);

	if (nodes->nodeTab[i]->type != XML_NAMESPACE_DECL)
	    nodes->nodeTab[i] = NULL;
    }
}

#else
int main(void) {
    fprintf(stderr, "XPath support not compiled in\n");
    exit(1);
}
#endif
