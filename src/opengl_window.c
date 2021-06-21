typedef struct sh_reloadable_shader {
	FILETIME last_write;
	i32 type;
	i32 handle;
	i8 needs_reload;
	char *file_name;

} sh_reloadable_shader;


i32 sh_create_shader(char *file_name, i32 type) {
	char *shader_source = read_file(file_name, NULL);
	i32 shader = glCreateShader(type);
	glShaderSource(shader, 1, &shader_source, NULL);
	glCompileShader(shader);

	GLint shader_compile_status;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &shader_compile_status);

	free(shader_source);
	if(shader_compile_status != GL_TRUE) {
		int log_size;
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_size);
		char *log = malloc(log_size);
		glGetShaderInfoLog(shader, log_size, &log_size, log);
		printf("shader (%s) has errors: %s\n", file_name, log);
		free(log);
		return 0;
	}

	return shader;
}

sh_reloadable_shader sh_create_reloadable_shader(char *file_name, i32 type) {
	sh_reloadable_shader shader = {0};

	shader.handle = sh_create_shader(file_name, type);

	if(shader.handle == 0) {
		printf("error creating reloadable shader.\n");
	} else {
		i32 file_name_size = (i32)strlen(file_name)+1;
		shader.file_name = malloc(file_name_size);
		CopyMemory(shader.file_name, file_name, file_name_size);
		shader.type = type;
		shader.last_write = sh_get_file_last_write(shader.file_name);
	}


	return shader;
}

i32 sh_create_program(i32 *shaders, i32 shader_count) {
	i32 program = glCreateProgram();
	for(int i = 0; i < shader_count; ++i) {
		glAttachShader(program, shaders[i]);
		// printf("shader %d\n", shaders[i]);
	}
	glLinkProgram(program);

	i32 program_link_status;
	glGetProgramiv(program, GL_LINK_STATUS, &program_link_status);

	if(program_link_status != GL_TRUE) {
		i32 log_length = 0;
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &log_length);

		char *log = malloc(log_length);
		glGetProgramInfoLog(program, log_length, &log_length, log);
		printf("program linking error(%d): %s\n", __LINE__, log);
		free(log);
		return 0;
	}

	return program;
}

void init_opengl() {

	GLuint vao = 0;
	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);

	sh_reloadable_shader vs = sh_create_reloadable_shader("shader/vertex_shader.glsl", GL_VERTEX_SHADER);
	sh_reloadable_shader fg = sh_create_reloadable_shader("shader/fragment_shader.glsl", GL_FRAGMENT_SHADER);

	// buf_push(gl_program_state.shaders, vs);
	// buf_push(gl_program_state.shaders, fg);

	i32 shaders[2] = {vs.handle, fg.handle};
	i32 texture_prog = sh_create_program(shaders, 2);
	// shaders[1] = fg2.handle;
	// gl_program_state.no_texture_prog = sh_create_program(shaders, 2);

	glUseProgram(texture_prog);

	gl_ctx.screen_scale = ortho(0, 500, 0, 500, -1.0, 1.0);
	glUniformMatrix4fv(3, 1, GL_FALSE, gl_ctx.screen_scale.m);
	glViewport(0, 0, gl_ctx.width, gl_ctx.height);

	glGenBuffers(1, &gl_ctx.vbo);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	glEnable(GL_MULTISAMPLE);
	// glPointSize(1);

	fflush(stdout);
}


void setup_opengl(HWND h_wnd) {
	PIXELFORMATDESCRIPTOR sh_px = {0};
	gl_ctx.hdc = GetDC(h_wnd);
	sh_px.nSize = sizeof(PIXELFORMATDESCRIPTOR);
	sh_px.nVersion = 1;
	sh_px.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
	sh_px.iPixelType = PFD_TYPE_RGBA;
	sh_px.cColorBits = 32;
	sh_px.cDepthBits = 24;
	sh_px.cStencilBits = 8;

	int index = ChoosePixelFormat(gl_ctx.hdc, &sh_px);
	SetPixelFormat(gl_ctx.hdc, index, &sh_px);

	HGLRC gl_temp = wglCreateContext(gl_ctx.hdc);
	wglMakeCurrent(gl_ctx.hdc, gl_temp);

	GLint attr[] = {
		WGL_CONTEXT_MAJOR_VERSION_ARB, 4,
		WGL_CONTEXT_MINOR_VERSION_ARB, 5,
		WGL_CONTEXT_PROFILE_MASK_ARB,
		WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
		0
	};

	load_ext();

	HGLRC gl_c = wglCreateContextAttribsARB(gl_ctx.hdc, 0, attr);
	wglMakeCurrent(gl_ctx.hdc, gl_c);
	load_ext();

	gl_ctx.hdc = GetDC(h_wnd);
	init_opengl();
}

LRESULT sh_window_proc(HWND h_wnd, UINT msg, WPARAM w_param, LPARAM l_param) {

	LRESULT result = 0; 

	switch(msg) {
		case WM_DESTROY: {
			PostQuitMessage(0);
			gl_ctx.should_close = 1;
		} break;

		case WM_KEYDOWN: {
			if(w_param == VK_ESCAPE) {
				gl_ctx.should_close = 1;
			}

			if(w_param == VK_UP) { gl_ctx.font_size++; }
			if(w_param == VK_DOWN) { gl_ctx.font_size--; }

			if(w_param == VK_LEFT) { gl_ctx.quad_height++; }
			if(w_param == VK_RIGHT) { gl_ctx.quad_height--; }


		} break;

		case WM_CHAR: {

			gl_ctx.input[gl_ctx.input_size] = (char)w_param;
			gl_ctx.input_size++;

		} break;

		default: result =  DefWindowProc(h_wnd, msg, w_param, l_param);
	}

	return result;
}

HWND sh_window_setup(void) {
	WNDCLASS wndclass = {0};
	wndclass.style = CS_OWNDC;
	wndclass.lpfnWndProc = (WNDPROC)sh_window_proc;
	wndclass.hInstance = NULL;
	wndclass.hbrBackground = (HBRUSH) (COLOR_BACKGROUND);
	wndclass.lpszClassName = "sh_gui";
	wndclass.hCursor = LoadCursor(NULL, IDC_ARROW);
	RegisterClass(&wndclass);

	RECT win_size = { .left = gl_ctx.x, .right = gl_ctx.width, .top = gl_ctx.y, .bottom = gl_ctx.height };

	AdjustWindowRect(&win_size, WS_OVERLAPPEDWINDOW, FALSE);

	HWND wn = CreateWindow(
			"sh_gui",
			gl_ctx.window_name,
			WS_VISIBLE | WS_OVERLAPPEDWINDOW,
			0,
			0,
			win_size.right - win_size.left,
			win_size.bottom - win_size.top,
			NULL,
			NULL,
			NULL,
			NULL
			);

	setup_opengl(wn);


	// init_time();
	// init_raw_input();
	return wn;
}

void handle_events() {
	MSG msg;
	while(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
}
