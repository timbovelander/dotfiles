{ File } = require('atom');

scratchPath = "#{process.env.ATOM_HOME}/scratch"
scratchFile = new File(scratchPath);
scratchFile.write("");

atom.commands.add 'atom-workspace', 'core:close-force', ->
  if atom.workspace.getActivePaneItem()
    atom.workspace.getActivePaneItem().destroy()
  else if atom.workspace.getPanes().length > 1
    atom.workspace.getActivePane().destroy()

atom.commands.add 'atom-text-editor', 'editor:copy-all', ->
  editor = atom.workspace.getActiveTextEditor()
  editor.selectAll()
  editor.copySelectedText()

atom.commands.add 'atom-text-editor', 'editor:delete-all', ->
  editor = atom.workspace.getActiveTextEditor()
  editor.selectAll()
  editor.delete()

atom.commands.add 'atom-text-editor', 'editor:scratch', ->
  atom.workspace.open(scratchPath)

atom.commands.add 'atom-workspace', 'pane:switch-next-recently-used-item', ->
  pane = atom.workspace.getActivePane()
  pane.activateNextRecentlyUsedItem()
  pane.moveActiveItemToTopOfStack()

atom.commands.add '.tree-view', 'tree-view:open-selected-entry-and-toggle', (ev) ->
  atom.commands.dispatch(ev.currentTarget, 'tree-view:open-selected-entry')
  if ev.currentTarget.querySelector('.selected').getAttribute('is') is 'tree-view-file'
    atom.commands.dispatch(ev.currentTarget, 'tree-view:toggle')
