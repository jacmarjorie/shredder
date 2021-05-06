/**
 * @fileoverview Define generation methods for custom blocks.
 * @author (Brandon Moore)
 */

// More on generating code:
// https://developers.google.com/blockly/guides/create-custom-blocks/generating-code

import * as Blockly from 'blockly/core';
import BlocklyJS from 'blockly/javascript';

BlocklyJS['text'] = function(block) {
    let textValue = block.getFieldValue('TEXT');
    return [textValue, 0]
}

/**
 *  NRC Generator
 */
BlocklyJS['forunion'] = function (block){
    const assignment = validate(block.getFieldValue('OBJECT_KEY'));
    const object = validate(block.getFieldValue( 'ATTRIBUTE_VALUE'));
    return `For ${assignment} In ${object} Union \n`
}

BlocklyJS['tuple'] = function (block){
    const attributes = Blockly.JavaScript.statementToCode(block, 'ATTRIBUTES');
    return `{(${attributes})}`;
}

BlocklyJS['tuple_el'] = function (block){
    const attribute_Name = validate(block.getFieldValue('ATTRIBUTE_NAME'))
    const attribute_value = validate(BlocklyJS.valueToCode(block,'ATTRIBUTE_VALUE',0));
    return `${attribute_Name} := ${attribute_value}`;
}

BlocklyJS['objects_attribute_list'] = function (block){
    const object_attribute = validate(block.getFieldValue('field_react_component'))
    return [object_attribute, 0];
    // return ['console.log(\'custom block\');\n', 0];
    // return 'console.log(\'custom block\');\n';
}

BlocklyJS['customReactComponent'] = function (block){
    return 'custom block\n';
}

BlocklyJS['tuple_el_iteration'] = function (block){
    const attribute_Name = validate(block.getFieldValue('ATTRIBUTE_NAME'))
    const attribute_value = validate(BlocklyJS.statementToCode(block,'ATTRIBUTE_VALUE',0));
    return `${attribute_Name} :=\n${attribute_value}`;
}

BlocklyJS['ifstmt_primitive'] = function (block){
    const object_association = validate(BlocklyJS.statementToCode(block,'OBJECT_ASSOCIATION'));
    const attributes = validate(BlocklyJS.statementToCode(block, 'ATTRIBUTES'));
    return [`If ${object_association} Then\n ${attributes}`, 0];
}

// Todo see above, conditional association, then a statement 
BlocklyJS['ifstmt_bag'] = function (block){
    const object_association = validate(BlocklyJS.statementToCode(block,'OBJECT_ASSOCIATION'));
    return `If ${object_association} Then\n`;
}

BlocklyJS['association_on'] = function (block){
    const object_associationA = validate(block.getFieldValue('ATTRIBUTE_A'));
    const object_associationB = validate(block.getFieldValue('ATTRIBUTE_A'));
    return `${object_associationA} == ${object_associationB}`;
}
BlocklyJS['association_on_assisted'] = function (block){
    const object_associationA = validate(block.getFieldValue('ATTRIBUTE_A'));
    const object_associationB = validate(block.getFieldValue('ATTRIBUTE_A'));
    return `${object_associationA} == ${object_associationB}`;
}
BlocklyJS['group_by'] = function (block){
    const attribute_key = validate(block.getFieldValue('ATTRIBUTE_KEY'))
    const attribute_value = validate(block.getFieldValue('ATTRIBUTE_VALUE'))
    return `GROUP BY ${attribute_key}_${attribute_value}`
}
BlocklyJS['brackets'] = function (block){
    const group_by = validate(BlocklyJS.statementToCode(block,'GROUP_BY'))
    return `\n(${group_by})`
}
BlocklyJS['or'] = function (block){
    return ' or '
}
BlocklyJS['and'] = function (block){
    return ' && '
}


const validate = (codeBlock) =>{
    return codeBlock?codeBlock:"UND";
}

