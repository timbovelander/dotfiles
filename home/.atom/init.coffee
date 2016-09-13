atom.commands.add 'atom-text-editor', 'e', ->
  atom.open()

atom.commands.add 'atom-text-editor', 'w', ->
  return unless editor = atom.workspace.getActiveTextEditor()
  editor.save()
