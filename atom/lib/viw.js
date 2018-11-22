'use babel';
import { CompositeDisposable } from 'atom';

import { Runner } from "../../target/scala-2.12/viw-fastopt.js"

export default {
  subscriptions: null,

  activate(state) {
    this.subscriptions = new CompositeDisposable();

    this.subscriptions.add(atom.commands.add('atom-text-editor', {
      'viw:processKey': (e) => {
        console.log(e);
        const s = this.getState();
        console.log(s)
        const result =
          Runner.processKey(e.originalEvent, s.content, s.position,
              s.selection, s.mode);

        if (result !== undefined) {
          console.log(result);
          this.processResult(result);
        } else {
          // Nothing has to be done by Viw; pass keypress.
          e.abortKeyBinding();
        }
      }
    }));
  },

  getState() {
    const editor = atom.workspace.getActiveTextEditor();
    const pos = editor.getCursorBufferPosition()
    return {
      "content": editor.getText(),
      "position": [pos.row, pos.column],
      "selection": editor.getSelectedBufferRange(),
      "mode": this.getEditorClassList().contains("viw-mode")
    }
  },

  processResult(result) {
    const editor = atom.workspace.getActiveTextEditor();
    if (editor.getText() !== result.content) {
      editor.setText(result.content);
    }
    editor.setCursorBufferPosition(result.position);
    if (result.selection !== undefined) {
      editor.setSelectedBufferRange(result.selection);
    }
    this.setViwMode(result.mode);
  },

  getEditor() {
    return atom.workspace.getActiveTextEditor();
  },

  getEditorClassList() {
    return this.getEditor().element.classList;
  },

  setViwMode(mode) {
    if (mode) {
      this.getEditorClassList().add("viw-mode");
    } else {
      this.getEditorClassList().remove("viw-mode");
    }
  },

  deactivate() {
    this.modalPanel.destroy();
    this.subscriptions.dispose();
    this.viwView.destroy();
  },

  serialize() {
  }
};
