const fs = require('fs');
const { createSVGWindow } = require('svgdom');
const { SVG, registerWindow } = require('@svgdotjs/svg.js');
const ELK = require('elkjs/lib/elk.bundled.js');

const window = createSVGWindow();
const document = window.document;
registerWindow(window, document);

var hdelk = (function() {

    /**
     * HDElkJS Style Section
     */

    var node_width = 75;
    var node_height = 75;

    var node_min_width = 20;
    var node_min_height = 20;

    var node_highlight_fill_color = ['#FFF', '#DDD', '#4bF','#F88', '#FE6','#7e0'];
    var node_fill_color = '#FFF';
    var node_stroke_color = '#666';
    var node_highlight_stroke_width = 2;
    var node_stroke_width = 1;
    var node_name_text_color = '#666';
    var node_highlight_name_text_color = [ '#DDD', '#222', '#46C', '#922', '#A90', '#350' ];
    var node_name_font_size = 16;
    var node_type_text_color = '#666';
    var node_type_font_size = 12;
    var node_label_width_padding = 4;
    var node_label_height_padding = 4;

    var node_port_name_font_size = 16;
    var node_port_height = 22;
    var node_port_width = 4;
    var node_port_name_text_color = '#FFF';
    var node_port_fill_color = '#777';

    var node_constant_notch = 10;
    var port_height = 18;
    var port_width_padding = 10;
    var port_name_font_size = 12;
    var port_fill_color = '#000';
    var port_text_color = '#FFF';
    var port_highlight_fill_color = [ '#DDD', '#444', '#06d', '#C00', '#980', '#590' ];
    var port_spacing = 4;

    var edge_label_text_size = 12;
    var edge_label_fill_color = '#EEE';
    var edge_label_text_color = '#777';
    var edge_label_width_padding = 4;
    var edge_label_height_padding = 4;
    var edge_label_highlight_fill_color = [ '#DDD', '#222', '#46C', '#922', '#A90', '#350' ];

    var edge_width = 1;
    var edge_color = '#888';
    var edge_highlight_color = [ '#DDD', '#444', '#06d', '#C00', '#980', '#590' ];
    var edge_highlight_width = 2;
    var edge_bus_width = 6;
    var edge_bus_color = '#AAA';
    var edge_bus_highlight_color = [ '#DDD', '#444', '#06d', '#C00', '#980', '#590' ];
    var edge_bus_highlight_width = 6;

    /**
     * Creates an SVG diagram from a JSON description.
     * @param {object} graph
     * @param {string} outputFilePath
     */
    var layout = async function(graph, outputFilePath) {
        const elk = new ELK();

        // create a dummy drawing just to get text sizes
        var drawDummy = SVG(document.documentElement).size(0, 0);

        transformNode(drawDummy, graph);

        drawDummy.clear();

        try {
            var g = await elk.layout(graph);

            var svgContent = diagram(g);

            fs.writeFileSync(outputFilePath, svgContent);
            console.log(`SVG file saved to ${outputFilePath}`);
        } catch (err) {
            console.error(err);
        }
    };

    /**
     * Takes the child object and recursively transforms sub-objects into a form that Elk.JS can use
     * @param {object} child present child node under consideration
     */
    var transformNode = function( drawDummy, child ) {

        if ( !child.layoutOptions )
            child.layoutOptions = {};

        if ( !child.layoutOptions[ 'elk.nodeLabels.placement' ] ) {
            if ( child.children && child.children.length > 0 )
                child.layoutOptions[ 'elk.nodeLabels.placement' ] = 'V_TOP H_CENTER INSIDE';
            else
                child.layoutOptions[ 'elk.nodeLabels.placement' ] = 'V_CENTER H_CENTER INSIDE'; // 'V_TOP H_LEFT INSIDE';
        }

        if ( !child.layoutOptions[ 'elk.portConstraints' ] ) {
            child.layoutOptions[ 'elk.portConstraints' ] = "FIXED_SIDE";
        }

        if ( !child.layoutOptions[ 'elk.nodeSize.constraints' ] ) {
            child.layoutOptions[ 'elk.nodeSize.constraints' ] = "NODE_LABELS PORTS MINIMUM_SIZE";
        }

        if ( !child.layoutOptions[ 'elk.spacing.portPort' ] ) {
            child.layoutOptions[ 'elk.spacing.portPort' ] = port_spacing;
        }

        if ( !child.layoutOptions['elk.nodeSize.options'] ) {
           child.layoutOptions['elk.nodeSize.options'] = '(' + node_min_width + ',' + node_min_height + ')';
       }
/*
        if ( !child.layoutOptions[ 'elk.layered.nodePlacement.networkSimplex.nodeFlexibility.default' ] ) {
            child.layoutOptions[ 'elk.layered.nodePlacement.networkSimplex.nodeFlexibility.default' ] = "PORT_POSITION NODE_SIZE";
        }

        if ( !child.layoutOptions[ 'elk.layered.nodePlacement.networkSimplex.nodeFlexibility' ] ) {
            child.layoutOptions[ 'elk.layered.nodePlacement.networkSimplex.nodeFlexibility' ] = "PORT_POSITION NODE_SIZE";
        }

        */
        if ( !child.id && child.id != "" )
            child.id = "";

        if ( !child.label && child.label != "" )
            child.label = child.id

        if ( child.port ) {
            // if ( !child.height )
            //     child.height = node_port_height;
            //     if ( child.ports )
            //         child.height += ( child.ports.length - 1 ) * port_height;
            // if ( !child.width ) {
            //     var tempText = drawDummy.text(child.label).font({ size: node_port_name_font_size });
            //     child.width = tempText.node.getComputedTextLength();
            //     if ( child.width == 0 )
            //        child.width = 6;
            // }
            if ( !child.color )
                child.color = (child.highlight || child.highlight == 0 ) ? port_highlight_fill_color[ child.highlight ]: node_port_fill_color;
        }

        if ( !child.labels ) {
            child.labels = [ ];
        }

        if ( child.label ) {
            child.labels.push( { text:child.label } );
        }

        if ( child.type ) {
            child.labels.push( { text:child.type, type:1 } );
        }

        var labels = child.labels;
        var calculatedNodeWidth = ( child.port ) ? node_port_width : node_min_width;
        var calculatedNodeHeight = ( child.port ) ? node_port_height : node_min_height;
        var labelHeight = 5;
        if ( labels ) {
            labels.forEach( function( item, index ) {
                var text = ( item.text ) ? item.text : "";
                var fontSize;
                if ( child.port )
                    fontSize = node_port_name_font_size;
                else
                    fontSize = ( item.type ) ? node_type_font_size : node_name_font_size;
                item.height = fontSize + node_label_height_padding;
                var tempText = drawDummy.text(text).font({ size: fontSize });
                item.width = tempText.node.getComputedTextLength() + node_label_width_padding;
                if ( item.width + 10 > calculatedNodeWidth )
                    calculatedNodeWidth = item.width + node_label_width_padding;
                labelHeight += item.height + node_label_height_padding;
            } );
            if ( labelHeight > calculatedNodeHeight )
                calculatedNodeHeight = labelHeight;
        }

        if ( !child.color && ( child.highlight || child.highlight == 0 ) ) {
            child.color = node_highlight_fill_color[ child.highlight ];
        }

        // child.widths are broken when labels are left justified
        // when they're centered, all is well.
        // leaving this here in case we like left justified labels in the future.
        // if ( !child.width )
        //      child.width = calculatedNodeWidth;
        // if ( !child.height )
        //     child.height = calculatedNodeHeight;

        if ( !child.ports )
            child.ports = [];

        var ports = child.ports;

        var inPorts = child.inPorts;
        if ( inPorts ) {
            inPorts.forEach( function( item, index ){
                if ( typeof( item ) == "string" ) {
                    var newItem = { id:item };
                    item = newItem;
                }
                ports.unshift( item );
                if ( !item.layoutOptions )
                    item.layoutOptions = {};

                if ( !item.layoutOptions[ 'elk.port.side' ] )
                    item.layoutOptions[ 'elk.port.side' ] = 'WEST'
             } );
        }

        var westPorts = child.westPorts;
        if ( westPorts ) {
            westPorts.forEach( function( item, index ){
                if ( typeof( item ) == "string" ) {
                    var newItem = { id:item };
                    item = newItem;
                }
                ports.unshift( item );
                if ( !item.layoutOptions )
                    item.layoutOptions = {};

                if ( !item.layoutOptions[ 'elk.port.side' ] )
                    item.layoutOptions[ 'elk.port.side' ] = 'WEST'
             } );
        }

        var eastPorts = child.eastPorts;
        if ( eastPorts ) {
            eastPorts.forEach( function( item, index ){
                if ( typeof( item ) == "string" ) {
                    var newItem = { id:item };
                    item = newItem;
                }
                ports.push( item );
                if ( !item.layoutOptions )
                    item.layoutOptions = {};

                if ( !item.layoutOptions[ 'elk.port.side' ] )
                    item.layoutOptions[ 'elk.port.side' ] = 'EAST'
             } );
        }

        var northPorts = child.northPorts;
        if ( northPorts ) {
            northPorts.forEach( function( item, index ){
                if ( typeof( item ) == "string" ) {
                    var newItem = { id:item };
                    item = newItem;
                }
                ports.push( item );
                if ( !item.layoutOptions )
                    item.layoutOptions = {};

                item.vertical = 1;

                if ( !item.layoutOptions[ 'elk.port.side' ] )
                    item.layoutOptions[ 'elk.port.side' ] = 'NORTH'
             } );
        }

        var southPorts = child.southPorts;
        if ( southPorts ) {
            southPorts.forEach( function( item, index ){
                if ( typeof( item ) == "string" ) {
                    var newItem = { id:item };
                    item = newItem;
                }
                ports.push( item );
                if ( !item.layoutOptions )
                    item.layoutOptions = {};

                item.vertical = 1;

                if ( !item.layoutOptions[ 'elk.port.side' ] )
                    item.layoutOptions[ 'elk.port.side' ] = 'SOUTH'
             } );
        }

        var outPorts = child.outPorts;
        if ( outPorts ) {
            outPorts.forEach( function( item, index ){
                if ( typeof( item ) == "string" ) {
                    var newItem = { id:item };
                    item = newItem;
                }
                ports.push( item );
                if ( !item.layoutOptions )
                    item.layoutOptions = {};

                if ( !item.layoutOptions[ 'elk.port.side' ] )
                    item.layoutOptions[ 'elk.port.side' ] = 'EAST'
             } );
        }

        var parameters = child.parameters;
        if ( parameters ) {
            parameters.forEach( function( item, index ){
                if ( typeof( item ) == "string" ) {
                    var newItem = { id:item };
                    item = newItem;
                }
                ports.push( item );

                item.param = 1;
                item.vertical = 1;

                if ( !item.layoutOptions )
                    item.layoutOptions = {};

                if ( !item.layoutOptions[ 'elk.port.side' ] )
                    item.layoutOptions[ 'elk.port.side' ] = 'NORTH'
                if ( !item.layoutOptions[ 'elk.port.index' ] )
                    item.layoutOptions[ 'elk.port.index' ] = ""+index
             } );
        }

        // there must be ports by now!
        ports.forEach( function( item, index, ports ){
            if ( typeof( item ) == "string" ) {
                item = { id:item }
                ports[ index ] = item;
            }
            if ( !item.id.includes(".") ) {
                if ( !item.label && item.label != "" )
                    item.label = item.id;
                item.id = child.id + "." + item.id;
            }
            if ( !item.label && item.label != "" )
                item.label = item.id;

            if ( !item.layoutOptions )
                item.layoutOptions = {}

            if ( !item.layoutOptions[ 'elk.port.side' ] )
                item.layoutOptions[ 'elk.port.side' ] = 'SIDES_EAST_WEST'

            if ( !item.width ) {
                var tempText = drawDummy.text(item.label).font({ size: port_name_font_size });
                item.width = tempText.node.getComputedTextLength() + port_width_padding;
            }
            if ( !item.height )
                item.height = port_height;

            // swap!
            if ( item.vertical ) {
                var t = item.width;
                item.width = item.height;
                item.height = t;
            }
        } )

        var edges = child.edges;
        if ( edges ) {
            edges.forEach( function( item, index, edges ) {
                if ( Array.isArray( item ) ) {
                    var newItem = {  }
                    edges[ index ] = newItem;
                    newItem.sources = [ item[ 0 ] ];
                    newItem.targets = [ item[ 1 ] ];
                    if ( item[ 2 ] ) {
                        if ( typeof( item[2] ) == "string" )
                            newItem.label = item[ 2 ];
                        else if ( item[ 2 ] == -1 ) {
                            newItem.reverse = 1;
                        } else
                            if ( item[2 ] == 1 )
                                newItem.bus = 1;
                    }
                    if ( item[ 3 ] ) {
                        if ( typeof( item[3] ) == "string" )
                            newItem.label = item[ 3 ];
                        else if ( item[ 3 ] == -1 ) {
                            newItem.reverse = 1;
                        } else
                            if ( item[ 3 ] == 1 )
                                newItem.bus = 1;
                    }
                    if ( item[ 4 ] ) {
                        if ( typeof( item[4] ) == "string" )
                            newItem.label = item[ 4 ];
                        else if ( item[ 4 ] == -1 ) {
                            newItem.reverse = 1;
                            // flip the source and target
                        } else
                            if ( item[ 4 ] == 1 )
                                newItem.bus = 1;
                    }
                    item = newItem;
                }
                if ( !item.id )
                    item.id = child.id + "E" + index;
                if ( !item.sources && item.source )
                    item.sources = [ item.source ];
                if ( !item.targets && item.target )
                    item.targets = [ item.target ];
                if ( ( !item.sources || !item.targets ) && item.route ) {
                    item.sources = [ item.route[ 0 ] ];
                    item.targets = [ item.route[ 1 ] ];
                }
                if ( item.reverse ) {
                    var s = item.sources;
                    item.sources = item.targets;
                    item.targets = s;
                }
                if ( !item.labels && item.label ) {
                    item.labels = [ { text:item.label } ];
                }
                var labels = item.labels;
                if ( labels ) {
                    labels.forEach( function( item, index ) {
                        if ( typeof( item ) == "string" ) {
                            var newItem = { text:item }
                            labels[ index ] = newItem;
                            item = newItem;
                        }
                        if ( ( item.text || item.text == "" ) && !item.width && !item.height ) {
                            var tempText = drawDummy.text(item.text).font({ size: edge_label_text_size });
                            item.width = tempText.node.getComputedTextLength() + edge_label_width_padding;
                            item.height = edge_label_text_size + edge_label_height_padding;
                        }
                    })
                }
            } );
        }

        var children = child.children;
        if ( children ) {
            children.forEach( function( item, index ) {
                transformNode( drawDummy, item  );
            } );
        }
    }

    /**
     * Takes the output from ElkJS, renders it into SVG using SVG.js and returns the result
     * @param {elkObject} diagram_layout
     * @returns {string} svg
     */
    var diagram = function(diagram_layout) {
        var draw = SVG(document.documentElement)
            .viewbox(0, 0, diagram_layout.width, diagram_layout.height)
            .size(diagram_layout.width, diagram_layout.height);

        node(draw, diagram_layout, 0, 0);

        return draw.svg();
    };

    var node = function( draw, child, offsetX, offsetY ) {
        var group = draw.group();

        var childColor;
        if ( child.color )
            childColor = child.color;
        else
            childColor = node_fill_color;

        var portColor = ( child.highlight || child.highlight == 0 ) ? port_highlight_fill_color[ child.highlight ] : port_fill_color;

        node_body( group, child.id, child.x + offsetX, child.y + offsetY, child.width, child.height, childColor, child.highlight, portColor, child.constant, child['stroke-width'] );

        var labels = child.labels;
        if ( labels ) {
            labels.forEach( function( item, index ){
                // group.rect( item.width, item.height ).attr({ fill:"#EEE" }).move(offsetX + child.x+item.x, offsetY + child.y+item.y );

                var labelText = ( ( item.text || item.text == "" ) ? item.text : item.id );
                var nameSize;
                var nameColor;
                if ( child.port ) {
                    nameSize = node_port_name_font_size;
                    nameColor = node_port_name_text_color;
                }
                else {
                    nameSize = node_name_font_size;
                    nameColor = ( child.highlight || child.highlight == 0 ) ? node_highlight_name_text_color[ child.highlight ] : node_name_text_color;
                }
               var nodeNameText;
                if ( item.type ) {
                    var typeColor = ( child.highlight == 0 ) ? nameColor : node_type_text_color;

                    nodeNameText = group.text(labelText).font({ size: node_type_font_size }).fill({color:typeColor});
                }
                else
                    nodeNameText = group.text(labelText).font({ size: nameSize }).fill({color:nameColor});
                if ( child.port ) {
                    var nodeNameTextWidth = nodeNameText.node.getComputedTextLength();
                    nodeNameText.move(offsetX + child.x+item.x+(item.width-nodeNameTextWidth)/2, offsetY + child.y+item.y + node_label_height_padding/2);
                }
                else
                    nodeNameText.move(offsetX + child.x + item.x, offsetY + child.y + item.y );
            } );
        }

        var edges = child.edges;
        if ( edges ) {
            edges.forEach( function( item, index ) {
                edge( group, item, offsetX + child.x, offsetY + child.y );
            } );
        }

        var children = child.children;
        if ( children ) {
            children.forEach( function( item, index ) {
                node( group, item, child.x + offsetX, child.y + offsetY  );
            } );
        }

        var ports = child.ports;
        if ( ports ) {
            ports.forEach( function( item, index ){
                var portText;
                if ( item.label)
                    portText = item.label;
                else
                    portText = item.id;

                var strokeWidth;
                var strokeColor;
                var fillColor;
                var nameColor;

                if ( item.param ) {
                    nameColor = ( child.highlight || child.highlight == 0 ) ? node_highlight_name_text_color[ child.highlight ] : node_name_text_color;
                    strokeWidth = child.highlight ? node_highlight_stroke_width : node_stroke_width;
                    strokeColor = portColor;
                    fillColor = childColor;
                } else {
                    nameColor = port_text_color;
                    strokeWidth = 0;
                    strokeColor = portColor;
                    fillColor = portColor;
                }

                group.rect(item.width, item.height).move(offsetX + child.x+item.x,offsetY + child.y+item.y)
                                                   .attr({ fill:fillColor, 'stroke-width': strokeWidth, stroke:strokeColor })
                                                   .stroke({width:strokeWidth});
                var portTextItem = group.text(portText).font({ size: port_name_font_size }).fill({color:nameColor});
                var portTextWidth = portTextItem.node.getComputedTextLength();


                if ( item.vertical ) {
                    //group.rect(item.width, item.height).move(offsetX + child.x+item.x,offsetY + child.y+item.y)
                    //                                   .attr({ fill:childColor, 'stroke-width': node_stroke_width, stroke:portColor })
                    //                                   .stroke({width:strokeWidth});
                    //var portTextItem = group.text(portText).font({ size: port_name_font_size }).fill({color:nameColor});
                    //var portTextWidth = portTextItem.node.getComputedTextLength();
                    portTextItem.transform({ rotate: 90, ox: 0, oy: 0 }).move( offsetY + child.y+item.y+(item.height-portTextWidth)/2, -(offsetX + child.x+item.x+item.width-(item.width-port_name_font_size)/2 + 2) );
                }
                else {
                    //group.rect( item.width, item.height ).attr({ fill:portColor }).move(offsetX + child.x+item.x, offsetY + child.y+item.y );
                    //var portTextItem = group.text(portText).font({ size: port_name_font_size }).fill({color:port_text_color});
                    //var portTextWidth = portTextItem.node.getComputedTextLength();
                        // draw the background
                    portTextItem.move(offsetX + child.x+item.x+(item.width-portTextWidth)/2, offsetY + child.y+item.y + 2);
                }
            } )
        }

        return group;
    }

    var node_body = function( draw, name, x, y, width, height, color, highlight, stroke_color, constant, customStrokeWidth ) {
        var group = draw.group();
        var strokeWidthValue = highlight ? node_highlight_stroke_width : (customStrokeWidth !== undefined ? customStrokeWidth : node_stroke_width);
        var shape;
        if ( constant ) {
            shape = group.polygon( [[0,0],[width-node_constant_notch,0],[width,node_constant_notch],[width,height],[0,height]]);
        } else {
            shape = group.rect(width, height);
        }
        shape.attr({ fill:color, 'stroke-width': strokeWidthValue, stroke:stroke_color }).stroke({width:strokeWidthValue}).move(x,y);
        return group;
    }

    var edge = function( draw, edge, offsetX, offsetY ) {
        var group = draw.group();

        var sections = edge.sections;

        var width;
        var color;
        if ( typeof edge.highlight !== 'undefined' ) {
            if ( edge.bus ) {
                width = edge_bus_highlight_width;
                color = edge_bus_highlight_color[ edge.highlight ];
            } else {
                width = edge_highlight_width;
                color = edge_highlight_color[ edge.highlight ];
            }
        } else {
            if ( edge.bus ) {
                width = edge_bus_width;
                color = edge_bus_color;
            } else {
                width = edge_width;
                color = edge_color;
            }
        }

        //         var width = ( edge.bus ) ? ( edge.highlight ?  edge_bus_highlight_width : edge_bus_width ) : ( edge.highlight ? edge_highlight_width : edge_width );
        // var color = ( edge.bus ) ? ( edge.highlight ? edge_bus_highlight_color[ edge.highlight ] : edge_bus_color ): ( edge.highlight ? edge_highlight_color[ edge.highlight ] : edge_color );

        if ( sections ) {
            sections.forEach( function( item, index ) {
                var startPoint = item.startPoint;
                var endPoint = item.endPoint;

                var bendPoints = item.bendPoints;

                if ( bendPoints == null ) {
                    group.line( offsetX + startPoint.x, offsetY + startPoint.y, offsetX + endPoint.x, offsetY + endPoint.y ).stroke( { color:color, width:width });
                } else {
                    var segments = [];
                    segments.push( [ offsetX + startPoint.x, offsetY + startPoint.y ] );
                    bendPoints.forEach( function( item ) {
                        segments.push( [ offsetX + item.x, offsetY + item.y ] );
                    } );
                    segments.push( [ offsetX + endPoint.x, offsetY + endPoint.y ] );
                    group.polyline( segments ).fill('none').stroke( { color:color, width:width } );
                }

                var terminatorWidth_2 = width;
                if ( terminatorWidth_2 < 3 )
                    terminatorWidth_2 = 3;
                if ( edge.reverse )
                    group.rect( terminatorWidth_2 * 2, terminatorWidth_2 * 2).attr({ fill:color }).move(offsetX + startPoint.x - terminatorWidth_2, offsetY + startPoint.y - terminatorWidth_2 );
                else
                    group.rect( terminatorWidth_2 * 2, terminatorWidth_2 * 2).attr({ fill:color }).move(offsetX + endPoint.x - terminatorWidth_2, offsetY + endPoint.y - terminatorWidth_2 );

            } );
        }

        var labels = edge.labels;
        if ( labels ) {

            var label_color = ( edge.highlight || edge.highlight == 0 ) ? edge_label_highlight_fill_color[ edge.highlight ] : edge_label_text_color;

            labels.forEach( function( item, index ) {

                // Handy for debugging layout
                // group.rect( item.width, item.height ).attr({ fill:edge_label_color }).move(offsetX + item.x, offsetY +item.y );

                var edgeText;
                if ( item.text)
                    edgeText = item.text;
                else
                    edgeText = item.id;
                var edgeTextItem = group.text(edgeText).font({ size: edge_label_text_size }).fill({color:label_color});
                var edgeTextWidth = edgeTextItem.node.getComputedTextLength();
                edgeTextItem.move(offsetX + item.x+(item.width-edgeTextWidth)/2, offsetY + item.y + edge_label_height_padding/2);

            })
        }

    }
    return {
        layout: layout
    };
})();

const inputFilePath = process.argv[2];
const outputFilePath = process.argv[3];

const graph = JSON.parse(fs.readFileSync(inputFilePath, 'utf8'));

hdelk.layout(graph, outputFilePath);