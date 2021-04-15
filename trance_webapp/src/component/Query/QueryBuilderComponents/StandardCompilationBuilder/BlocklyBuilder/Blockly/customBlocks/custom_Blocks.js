import Blockly from 'blockly';

Blockly.defineBlocksWithJsonArray([{
        "type": "for_loop",
        "message0": "for %1 in %2 union",
        "args0": [
            {
                "type": "field_input",
                "name": "OBJECT_KEY",
                "text": ""
            },
            {
                "type": "field_input",
                "name": "ATTRIBUTE_VALUE"
            },

        ],
        "colour": 142,
        "nextStatement": true,
        "previousStatement": true,
        "tooltip": "for_loop",

    },
    {
        "type": "tuple",
        "message0": "{(%1)}",
        "args0": [
            {
                "type": "input_statement",
                "name": "ATTRIBUTES"
            }
        ],
        "colour": 255,
        "previousStatement": null,
        "nextStatement": null,
        "tooltip": "tuple",

    },
    {
        "type": "brackets",
        "message0": "(%1)",
        "args0": [
            {
                "type": "input_statement",
                "name": "GROUP_BY"
            }
        ],
        "colour": 33,
        "previousStatement": true,
        "nextStatement": true,
        "tooltip": "brackets",
    },
    {
        "type": "or",
        "message0": "||",
        "colour": 50,
        "previousStatement": null,
        "nextStatement": null,
        "tooltip": "or",

    },
    {
        "type": "and",
        "message0": "&&",
        "colour": 142,
        "previousStatement": null,
        "nextStatement": null,
        "tooltip": "and",

    },
    {
        "type": "tuple_el",
        "message0": "%1 %2 %3",
        "args0": [
            {
                "type": "field_input",
                "name": "ATTRIBUTE_NAME",
                "text": ""
            },
            {
                "type": "field_label",
                "name": "COLON",
                "text": ":="
            },
            {
                "type": "input_value",
                "name": "ATTRIBUTE_VALUE"
            }
        ],
        "colour": 111,
        "previousStatement": null,
        "nextStatement": null,
        "tooltip": "tuple_el",
    },
    {
        "type": "tuple_el_iteration",
        "message0": "%1 %2 %3",
        "args0": [
            {
                "type": "field_input",
                "name": "ATTRIBUTE_NAME",
                "text": ""
            },
            {
                "type": "field_label",
                "name": "COLON",
                "text": ":="
            },
            {
                "type": "input_statement",
                "name": "ATTRIBUTE_VALUE"
            }
        ],
        "colour": 230,
        "previousStatement": null,
        "nextStatement": null,
        "tooltip": "tuple_el_iteration",
    },
    {
        "type": "object_association",
        "message0": "if %1 then",
        "args0": [
            {
                "type": "input_statement",
                "name": "OBJECT_ASSOCIATION"
            }
        ],
        "colour": 23,
        "previousStatement": null,
        "nextStatement": null,
        "tooltip": "object_association",

    },
    {
        "type": "association_on",
        "message0": "%1 %2 %3",
        "args0": [
            {
                "type": "field_input",
                "name": "ATTRIBUTE_A",
                "text": ""
            },
            {
                "type": "field_label",
                "name": "COLON",
                "text": "=="
            },
            {
                "type": "field_input",
                "name": "ATTRIBUTE_B",
                "text": ""
            }
        ],
        "colour": 142,
        "previousStatement": null,
        "nextStatement": null,
        "tooltip": "association_on",
    },
    {
        "type": "group_by",
        "message0": "Group by %1_%2 ",
        "args0": [
            {
                "type": "field_input",
                "name": "ATTRIBUTE_KEY",
                "text": ""
            },
            {
                "type": "field_input",
                "name": "ATTRIBUTE_VALUE",
                "text": ""
            }
        ],
        "colour": 130,
        "previousStatement": true,
        "nextStatement": true,
        "tooltip": "group_by",
    }
]);