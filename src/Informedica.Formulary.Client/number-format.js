import React from 'react';
import NumberFormat from 'react-number-format';

function merge(o1, o2) {
    const o = { ...o1, ...o2 };
    console.log("merged", o);
    // although inputRef is a member of the object, it cannot be removed
    // because of inputRef is not defined error
    // return { inputRef, ..o };
    return o;
}

export { merge, NumberFormat as default };