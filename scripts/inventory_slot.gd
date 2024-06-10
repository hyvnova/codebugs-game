### Inventory_Slot.gd

extends Control

# Scene-Tree Node references
@onready var icon = $InnerBorder/ItemIcon
@onready var quantity_label = $InnerBorder/ItemQuantity

# Details 
# * This needs to be modified if more propperties are added to ItemData
@onready var details_panel = $DetailsPanel
@onready var item_type = $DetailsPanel/ItemType

# Usage panel
@onready var usage_panel = $UsagePanel

# Slot item_data
var item_data: ItemData = null

# Show usage panel for player to use/remove item_data
func _on_item_button_pressed():
	if item_data:
		usage_panel.visible = !usage_panel.visible

# Show item_data details on hover enter
# Details are things like type, quantity
func _on_item_button_mouse_entered():
	if item_data:
		usage_panel.visible = false
		details_panel.visible = true

# Hide item_data details on hover exit
func _on_item_button_mouse_exited():
	details_panel.visible = false

# Default empty slot
func set_empty():
	icon.texture = null
	quantity_label.text = ""
	usage_panel.visible = false

# Set slot item_data with its values from the dictionary
func set_item(data: ItemData):
	item_data = data
	icon.texture = item_data.texture
	quantity_label.text = str(item_data.quantity)
	item_type.text = str(item_data.type)


# Remove item_data from inventory and drop it back into the world        		
func _on_drop_button_pressed():
	if item_data:
		var drop_position = Global.player_node.global_position
		var drop_offset = Vector2(10, 50)
		drop_offset = drop_offset.rotated(Global.player_node.rotation)

		Global.drop_item(item_data.clone(), drop_position + drop_offset)
		Global.remove_item(item_data.type)

		usage_panel.visible = false

# Remove item_data from inventory, use it, and apply its effect (if possible)		
func _on_use_button_pressed():
	usage_panel.visible = false
	if item_data:
		if Global.player_node:
			# ! USE ITEM
			
			Global.remove_item(item_data.type)
		else:
			print("Player could not be found")
