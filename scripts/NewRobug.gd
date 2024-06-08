extends Button


func _on_pressed():
	# Get robug spawner (TODO: get closest or let user select yk this shit)
	var spawn_pos: Vector2 =  (get_tree().get_nodes_in_group("robug_spawn")[0] as Node2D).global_position
	
	# Spawn a new robug
	get_tree().get_nodes_in_group("world")[0].add_child(
		Robug.spawn(spawn_pos)
	)
