--- xterm-322.ori/button.c	2017-07-13 18:55:15.892861414 +0200
+++ xterm-322/button.c	2017-07-13 19:00:35.216640602 +0200
@@ -2980,10 +2980,10 @@
 #endif
 
 static int
-class_of(LineData *ld, CELL *cell)
+char_at(LineData *ld, CELL *cell)
 {
     CELL temp = *cell;
-    int result = 0;
+    int result = -1;
 
 #if OPT_DEC_CHRSET
     if (CSET_DOUBLE(GetLineDblCS(ld))) {
@@ -2991,7 +2991,20 @@
     }
 #endif
     if (temp.col < (int) ld->lineSize)
-	result = CharacterClass((int) (ld->charData[temp.col]));
+      result = (int) (ld->charData[temp.col]);
+    return result;
+}
+
+static int
+class_of(LineData *ld, CELL *cell)
+{
+    int result = 0;
+
+    result = char_at(ld, cell);
+    if (result != -1)
+      result = CharacterClass(result);
+    else
+      result = 0;
     return result;
 }
 
@@ -3006,6 +3019,8 @@
 
 #define CClassOf(name) class_of(ld.name, &((screen->name)))
 
+#define CharAT(name) char_at(ld.name, &((screen->name)))
+
 /*
  * If the given column is past the end of text on the given row, bump to the
  * beginning of the next line.
@@ -3463,6 +3478,113 @@
 	screen->saveStartW = screen->startSel;
 	break;
 
+    case Select_BRACKET:		/* brackets */
+
+      TRACE(("Select_BRACKET\n"));
+	int open = CharAT(startSel);
+	int close = 0;
+	switch (open) {
+	case '}':
+	case ')':
+	case ']':
+	  close = 1;
+	  if (okPosition(screen, &(ld.startSel), &(screen->startSel))) {
+	    CELL e_save = screen->startSel;
+
+	    while (close > 0) {
+
+	      if (--screen->startSel.col < 0) {
+		while (screen->startSel.row > 0) {
+		  PrevRow(startSel);
+		  length = LastTextCol(screen, ld.startSel, screen->startSel.row);
+		  if (length >= 0) {
+		    screen->startSel.col = length;
+		    break;
+		  }
+		}
+		if (screen->startSel.col < 0) {
+		  screen->startSel = e_save;
+		  break;
+		}
+	      }
+
+	      int f = CharAT(startSel);
+	      switch (f) {
+	      case '(':
+	      case '{':
+	      case '[':
+		if ((open == ')' && f == '(') ||
+		    (open == '}' && f == '{') ||
+		    (open == ']' && f == '[')) {
+		  if (--close <= 0) {
+		    break;
+		  }
+		}
+	      case ')':
+	      case '}':
+	      case ']':
+		if (open == f)
+		  close++;
+		break;
+	      }
+	    }
+	  }
+
+
+	  break;
+
+	case '(':
+	case '{':
+	case '[':
+	  close = 1;
+	  if (okPosition(screen, &(ld.endSel), &(screen->endSel))) {
+	    CELL e_save = screen->endSel;
+
+	    length = LastTextCol(screen, ld.endSel, screen->endSel.row);
+
+	    while (close > 0) {
+	      ++screen->endSel.col;
+	      if (screen->endSel.col > length) {
+		if (!MoreRows(endSel)) {
+		  screen->endSel = e_save;
+		  break;
+		}
+		screen->endSel.col = 0;
+		NextRow(endSel);
+		length = LastTextCol(screen, ld.endSel, screen->endSel.row);
+	      }
+
+	      int f = CharAT(endSel);
+	      switch (f) {
+	      case '(':
+	      case '{':
+	      case '[':
+		if (open == f)
+		  close++;
+		break;
+	      case ')':
+	      case '}':
+	      case ']':
+		if ((open == '(' && f == ')') ||
+		    (open == '{' && f == '}') ||
+		    (open == '[' && f == ']')) {
+		  if (--close <= 0) {
+		    ++screen->endSel.col;
+		    break;
+		  }
+		}
+		break;
+	      }
+	    };
+	  }
+	  break;
+	default:
+	  goto do_line;
+	}
+
+	break;
+    do_line:
+
     case Select_LINE:
 	TRACE(("Select_LINE\n"));
 	while (LineTstWrapped(ld.endSel)
