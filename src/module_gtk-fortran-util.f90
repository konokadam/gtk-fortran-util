!!!Author H. E. Konokman
module gtk_fortran_util

    use :: iso_c_binding, only: c_ptr, c_int, c_null_char
    use g, only: g_free
    use gtk, only: gtk_box_new, gtk_container_add, gtk_entry_new,&
        & gtk_entry_set_text, gtk_file_chooser_button_new,&
        & gtk_file_chooser_get_filename, gtk_file_chooser_set_current_folder,&
        & gtk_file_chooser_set_filename, gtk_label_new, gtk_menu_button_new,&
        & gtk_menu_button_set_direction, gtk_menu_button_set_popover,&
        & gtk_menu_new_from_model, gtk_popover_new, gtk_radio_button_new,&
        & gtk_table_attach, gtk_widget_show_all, gtk_image_new_from_icon_name, &
        & gtk_entry_set_width_chars, gtk_button_new_from_icon_name, &
        & GTK_ARROW_RIGHT, GTK_ICON_SIZE_BUTTON
    use gtk_sup

    use :: gtk_hl_container
    use :: gtk_hl_entry
    use :: gtk_hl_chooser

    implicit none

    abstract interface
        subroutine clicked_changed_proc_int(widget, gdata) bind(c)
            import :: c_ptr
            type(c_ptr), value :: widget, gdata
        end subroutine
    end interface

    type :: clicked_changed_proc_type
        procedure(clicked_changed_proc_int), pointer, nopass :: proc
    end type

contains

    function filenamef(widget)

        type(c_ptr) :: widget
        character(len=:), allocatable :: filenamef
        character(len=1000) :: filename
        type(c_ptr) :: c_string

        c_string = gtk_file_chooser_get_filename(widget)
        call convert_c_string(c_string, filename)
        call g_free(c_string)

        filenamef = trim(filename)

    end function filenamef

    subroutine vector_line(table, irow, label_line, label_entry, &
            tooltip_entry, label_unit, entry_changed, values)
        type(c_ptr), intent(inout) :: table
        integer, intent(in) :: irow
        character(*), intent(in) :: label_line
        character(*), intent(in) :: label_entry(:)
        character(*), intent(in), optional :: tooltip_entry(:)
        character(*), intent(in) :: label_unit(:)
        type(clicked_changed_proc_type) :: entry_changed(:)
        character(*), intent(in), optional :: values(:)

        type(c_ptr) :: label_line_
        type(c_ptr) :: label_entry_
        type(c_ptr) :: entry_
        type(c_ptr) :: label_unit_
        integer :: i
        integer(c_int) :: ir, ic

        label_line_ = gtk_label_new(label_line//c_null_char)
        ir = int(irow,c_int)
        call hl_gtk_table_attach(table, label_line_, 0_c_int, ir, xopts=FALSE, yopts=FALSE)
        do i = 1, size(label_entry)
            label_entry_ = gtk_label_new(label_entry(i)//c_null_char)
            ic = int((i-1)*3+1,c_int)
            call hl_gtk_table_attach(table, label_entry_, ic, ir, xpad=20_c_int, xopts=FALSE, yopts=FALSE)
            if(present(tooltip_entry)) then
                entry_ = hl_gtk_entry_new(editable=TRUE, &
                    & tooltip=tooltip_entry(i)//c_null_char, &
                    & changed=c_funloc(entry_changed(i)%proc),&
                    & size=1_c_int)
                if(present(values)) call gtk_entry_set_text(entry_, values(i)//c_null_char)
            else
                entry_ = hl_gtk_entry_new(editable=TRUE, &
                    & changed=c_funloc(entry_changed(i)%proc), &
                    & size=1_c_int)
                if(present(values)) call gtk_entry_set_text(entry_, values(i)//c_null_char)
            endif
            call gtk_entry_set_width_chars(entry_, 8_c_int)

            call hl_gtk_table_attach(table, entry_, ic+1, ir, xopts=FALSE, yopts=FALSE)
            label_unit_ = gtk_label_new(label_unit(i)//c_null_char)
            call hl_gtk_table_attach(table, label_unit_, ic+2, ir, xopts=FALSE, yopts=FALSE)
        enddo

    end subroutine vector_line

    subroutine file_chooser_line_by_file_chooser_button(box_, label, file_chooser_title, filter, &
            & filter_name, create, file_set, file_name)
        type(c_ptr), intent(inout) :: box_
        character(*), intent(in) :: label
        character(*), intent(in) :: file_chooser_title
        character(*), intent(in) :: filter(:)
        character(*), intent(in) :: filter_name(:)
        integer(c_int), intent(in) :: create
        procedure(clicked_changed_proc_int) :: file_set
        character(*), intent(in), optional :: file_name

        type(c_ptr) :: label_
        type(c_ptr) :: file_chooser_button_
        integer :: lval !, ifile

        integer(c_int) :: fill, expand, padding
        expand = true
        fill = TRUE
        padding = TRUE

        box_ = hl_gtk_box_new(horizontal=TRUE, homogeneous=FALSE)

        label_ = gtk_label_new(label//c_null_char)
        call hl_gtk_box_pack(box_, label_, expand=FALSE, fill=FALSE, padding=FALSE)
        file_chooser_button_ = hl_gtk_file_chooser_button_new(title=file_chooser_title//c_null_char, &
            !            width=20_c_int, &
            filter=filter, filter_name=filter_name, &
            file_set=c_funloc(file_set))
        !        ifile = gtk_file_chooser_set_current_folder(file_chooser_button_, ""//c_null_char)
        if(present(file_name)) lval = gtk_file_chooser_set_filename(file_chooser_button_, &
            trim(file_name)//c_null_char)
        call hl_gtk_box_pack(box_, file_chooser_button_, expand=expand, fill=fill, padding=padding)

    end subroutine

    subroutine file_chooser_line_by_entry_button(box_, label, value, button_icon_name, &
            & entry_changed, button_clicked)
        type(c_ptr), intent(inout) :: box_
        character(*), intent(in) :: label
        character(*), intent(in), optional :: value
        character(*), intent(in), optional :: button_icon_name
        procedure(clicked_changed_proc_int) :: entry_changed
        procedure(clicked_changed_proc_int) :: button_clicked

        type(c_ptr) :: label_
        type(c_ptr) :: button
        type(c_ptr) :: entry
        integer :: lval !, ifile

        integer(c_int) :: fill, expand, padding
        expand = true
        fill = TRUE
        padding = TRUE

        box_ = hl_gtk_box_new(horizontal=TRUE, homogeneous=FALSE)

        label_ = gtk_label_new(label//c_null_char)
        call hl_gtk_box_pack(box_, label_, expand=FALSE, fill=FALSE, padding=FALSE)
        entry = hl_gtk_entry_new(value=value, changed=c_funloc(entry_changed))
        call hl_gtk_box_pack(box_, entry, expand=expand, fill=fill, padding=padding)
        button = gtk_button_new_from_icon_name(button_icon_name//c_null_char, 3_c_int)
        call g_signal_connect(button, "clicked"//c_null_char, c_funloc(button_clicked), entry)
        call hl_gtk_box_pack(box_, button, expand=expand, fill=fill, padding=padding)

    end subroutine

    function file_chooser_file(title, filters, filtnames, create, parent) result(file)

        character(*), intent(in) :: title
        character(*), dimension(:), intent(in) :: filters
        character(*), dimension(:), intent(in) :: filtnames
        integer(c_int), intent(in) :: create
        type(c_ptr), intent(in), optional :: parent

        character(len=:), allocatable :: file
        integer(kind=c_int) :: isel
        character(len=120), dimension(:), allocatable :: chfile

        isel = hl_gtk_file_chooser_show(chfile, create=create,&
            & title=title//c_null_char, &
            & current=TRUE, &
            & filter=filters, &
            & filter_name=filtnames, wsize=(/ 600_c_int, 400_c_int /), &
            & edit_filters=TRUE, &
            & parent=parent, all=TRUE)

        if (isel == FALSE) return   ! No selection made

        file = trim(chfile(1))
        deallocate(chfile)

    end function


    subroutine treelistview_add_line(tree, row, renderers, svalues, pbvalues)

        use :: gtk_hl_tree

        type(c_ptr), intent(inout) :: tree
        integer(c_int), intent(in) :: row(:)
        character(*), intent(in) :: renderers(:)
        character(*), intent(in), optional :: svalues(:)
        type(c_ptr), intent(in), optional :: pbvalues(:)

        integer :: isvalue, ipbvalue
        integer :: i


        call hl_gtk_tree_ins(tree, row=row)

        isvalue = 0
        ipbvalue = 0
        do i = 1, size(renderers)
            select case(trim(renderers(i)))
                case(hl_gtk_cell_text)
                    isvalue = isvalue + 1

                    call hl_gtk_tree_set_cell(tree, row=row, col=int(i-1,c_int), &
                        & svalue=trim(svalues(isvalue))//c_null_char)
                case(hl_gtk_cell_pixbuf)
                    ipbvalue = isvalue + 1

                    call hl_gtk_tree_set_cell(tree, row=row, col=int(i-1,c_int), &
                        & pbvalue=pbvalues(ipbvalue))
                case default
                    print*, "Treelisview column renderer type could not be determined!"
            end select
        enddo


    end subroutine

    function create_menu_button_with_empty_popover(box, label_mb, label_mb_c, icon_name, icon_size, popover, arrow_right) &
            result(menu_button)

        type(c_ptr), optional :: label_mb
        character(*), intent(in), optional :: label_mb_c
        character(*), intent(in), optional :: icon_name
        integer(c_int), intent(in), optional :: icon_size
        type(c_ptr), intent(inout) :: box
        type(c_ptr), optional :: popover
        integer(c_int), intent(in), optional :: arrow_right

        type(c_ptr) :: menu_button
        type(c_ptr) :: image_mb


        menu_button = gtk_menu_button_new()
        if(present(label_mb)) label_mb = gtk_label_new(label_mb_c//c_null_char)
        if(present(label_mb_c)) then
            call gtk_container_add(menu_button, label_mb)
        endif
        if(present(icon_name)) then
            if(present(icon_size)) then
                image_mb = gtk_image_new_from_icon_name(icon_name//c_null_char, icon_size)
            else
                image_mb = gtk_image_new_from_icon_name(icon_name//c_null_char, GTK_ICON_SIZE_BUTTON)
            endif
            call gtk_container_add(menu_button, image_mb)
        end if
        if(present(arrow_right)) then
            if(arrow_right == TRUE) call gtk_menu_button_set_direction(menu_button, GTK_ARROW_RIGHT)
        endif
        popover = gtk_popover_new(menu_button)
        call gtk_menu_button_set_popover(menu_button, popover)
        call gtk_container_add(popover, box)

        call gtk_widget_show_all(box)

    end function

    function menu_button_with_popover_filled_radio(group, label_mb_, label_mb_c, box_po, label_radio, toggled_rb, select) &
            result(menu_button)

        type(c_ptr) :: menu_button
        type(c_ptr), intent(out) :: group
        type(c_ptr), intent(inout) :: label_mb_
        character(*), intent(in) :: label_mb_c
        type(c_ptr), intent(out) :: box_po
        character(*), intent(in) :: label_radio(:)
        procedure(clicked_changed_proc_int) :: toggled_rb
        integer, intent(in), optional :: select

        type(c_ptr) :: popover, button
        integer :: i

        menu_button = gtk_menu_button_new()
        !menu = gtk_menu_new_from_model(menu_button)
        label_mb_ = gtk_label_new(label_mb_c//c_null_char)
        call gtk_container_add(menu_button, label_mb_)

        popover = gtk_popover_new(menu_button)
        call gtk_menu_button_set_popover(menu_button, popover)
        box_po = hl_gtk_box_new()
        call gtk_container_add(popover, box_po)

        do i = 1, size(label_radio)
            button = hl_gtk_radio_button_new(group, label=trim(label_radio(i))//c_null_char, &
                toggled=c_funloc(toggled_rb), data=popover)
            call hl_gtk_box_pack(box_po, button)
        enddo
        call gtk_widget_show_all(box_po)

        if(present(select)) call hl_gtk_radio_group_set_select(group, int(select-1, c_int))

    end function

end module


