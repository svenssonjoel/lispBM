/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "qt_extensions.h"
#include "QLbmWidget.h"
#include "QLbmContainerWidget.h"
#include "QLbmDisplayWidget.h"
#include "QLbmButtonWidget.h"
#include "QLbmStretchWidget.h"
#include "QLispBM.h"
#include "QLbmValue.h"

#include <QMetaObject>
#include <QImage>
#include <QHash>

extern "C" {
#include "lispbm.h"
#include "heap.h"
#include "extensions/display_extensions.h"
}

// ---------------------------------------------------------------------------
// Module state — single registry for all LBM-managed widgets

static QLbmContainerWidget           *s_root          = nullptr;
static int                            s_rootHandle    = -1;
static int                            s_nextHandle    = 0;
static QHash<int, QLbmWidget *>       s_widgets;       // all LBM-managed widgets
static QLbmDisplayWidget             *s_activeDisplay = nullptr;

void lbm_qt_extensions_set_widget(QLbmContainerWidget *widget) {
  s_root       = widget;
  s_rootHandle = s_nextHandle++;
  s_widgets.insert(s_rootHandle, widget);
}

// ---------------------------------------------------------------------------
// Helpers

static QLbmContainerWidget *getContainer(int handle) {
  return qobject_cast<QLbmContainerWidget *>(s_widgets.value(handle, nullptr));
}

static int registerWidget(QLbmWidget *w) {
  int handle = s_nextHandle++;
  s_widgets.insert(handle, w);
  return handle;
}

static QLbmLayout layoutFromSymbol(lbm_value sym) {
  const char *name = lbm_get_name_by_symbol(lbm_dec_sym(sym));
  if (name) {
    if (strcmp(name, "hbox") == 0) return QLbmLayout::HBox;
    if (strcmp(name, "grid") == 0) return QLbmLayout::Grid;
  }
  return QLbmLayout::VBox; // default
}

// ---------------------------------------------------------------------------
// Widget attribute system — mirrors the display library's optional attr args.
// Each attribute is passed as a trailing argument that is either:
//   a list  '(attr-name value ...)  e.g. '(max-width 60)
//   a symbol 'attr-name             for flag attributes with no value

struct QtWidgetAttrs {
  int max_width  = -1;
  int min_width  = -1;
  int max_height = -1;
  int min_height = -1;
};

static QtWidgetAttrs parseAttrs(lbm_value *args, lbm_uint argn, lbm_uint start) {
  QtWidgetAttrs a;
  for (lbm_uint i = start; i < argn; i++) {
    lbm_value attr = args[i];
    const char *key = nullptr;
    lbm_value   val = ENC_SYM_NIL;

    if (lbm_is_symbol(attr)) {
      key = lbm_get_name_by_symbol(lbm_dec_sym(attr));
    } else if (lbm_is_cons(attr)) {
      lbm_value head = lbm_car(attr);
      if (lbm_is_symbol(head)) {
        key = lbm_get_name_by_symbol(lbm_dec_sym(head));
        val = lbm_cdr(attr);
      }
    }
    if (!key) continue;

    if (strcmp(key, "max-width")  == 0 && lbm_is_cons(val))
      a.max_width  = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "min-width")  == 0 && lbm_is_cons(val))
      a.min_width  = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "max-height") == 0 && lbm_is_cons(val))
      a.max_height = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "min-height") == 0 && lbm_is_cons(val))
      a.min_height = (int)lbm_dec_as_i32(lbm_car(val));
  }
  return a;
}

static void applyAttrs(QLbmWidget *w, const QtWidgetAttrs &a) {
  if (a.max_width  >= 0) w->setMaximumWidth(a.max_width);
  if (a.min_width  >= 0) w->setMinimumWidth(a.min_width);
  if (a.max_height >= 0) w->setMaximumHeight(a.max_height);
  if (a.min_height >= 0) w->setMinimumHeight(a.min_height);
}

// ---------------------------------------------------------------------------
// Image format conversion

static QImage imageFromBuffer(const image_buffer_t *img) {
  int w = (int)img->width;
  int h = (int)img->height;

  switch (img->fmt) {
  case rgb888:
    return QImage(img->data, w, h, w * 3, QImage::Format_RGB888).copy();
  case rgb565:
    return QImage(img->data, w, h, w * 2, QImage::Format_RGB16).copy();
  case rgb332: {
    QImage out(w, h, QImage::Format_RGB32);
    for (int y = 0; y < h; y++) {
      for (int x = 0; x < w; x++) {
        uint8_t p = img->data[y * w + x];
        uint8_t r = (uint8_t)(((p >> 5) & 0x7u) * 255u / 7u);
        uint8_t g = (uint8_t)(((p >> 2) & 0x7u) * 255u / 7u);
        uint8_t b = (uint8_t)((p        & 0x3u) * 255u / 3u);
        out.setPixel(x, y, qRgb(r, g, b));
      }
    }
    return out;
  }
  default:
    return QImage();
  }
}

// ---------------------------------------------------------------------------
// render_image callback

static bool qt_render_image(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) {
  (void)colors;
  if (!s_activeDisplay) return false;

  QImage qimg = imageFromBuffer(img);
  if (qimg.isNull()) return false;

  QLbmDisplayWidget *pane = s_activeDisplay;
  int ix = (int)x;
  int iy = (int)y;
  QMetaObject::invokeMethod(pane, [pane, ix, iy, qimg]() {
    pane->setImageAt(ix, iy, qimg);
  }, Qt::BlockingQueuedConnection);

  return true;
}

// ---------------------------------------------------------------------------
// Shared logic for creating a display widget inside a container

static int createDisplay(QLbmContainerWidget *container, int w, int h,
                         const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, w, h, attrs, &handle]() {
    auto *display = new QLbmDisplayWidget(w, h, container);
    applyAttrs(display, attrs);
    handle = registerWidget(display);
    container->addChildWidget(display);
  }, Qt::BlockingQueuedConnection);

  if (handle < 0) return -1;
  s_activeDisplay = qobject_cast<QLbmDisplayWidget*>(s_widgets.value(handle, nullptr));
  lbm_display_extensions_set_callbacks(qt_render_image, nullptr, nullptr);
  return handle;
}

static int createButton(QLbmContainerWidget *container, const QString &label,
                        const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, label, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *btn = new QLbmButtonWidget(label, handle, container);
    applyAttrs(btn, attrs);
    s_widgets.insert(handle, btn);
    container->addChildWidget(btn);
    QObject::connect(btn, &QLbmButtonWidget::clicked, [](int h) {
      if (QLispBM::instance()) {
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("button-pressed"),
                               QLbmValue::fromI(h)}));
      }
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createContainer(QLbmContainerWidget *parent, QLbmLayout layout,
                           const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(parent, [parent, layout, attrs, &handle]() {
    auto *container = new QLbmContainerWidget(layout, parent);
    applyAttrs(container, attrs);
    handle = registerWidget(container);
    parent->addChildWidget(container);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createStretchInner(QLbmContainerWidget *container, Qt::Orientation orientation,
                              const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, orientation, attrs, &handle]() {
    auto *stretch = new QLbmStretchWidget(orientation, container);
    applyAttrs(stretch, attrs);
    handle = registerWidget(stretch);
    container->addChildWidget(stretch);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

// ---------------------------------------------------------------------------
// (qt-root) -> handle  [returns handle of the root container]

static lbm_value ext_qt_root(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (s_rootHandle < 0) {
    lbm_set_error_reason("qt-root: no root widget registered");
    return ENC_SYM_EERROR;
  }
  return lbm_enc_i(s_rootHandle);
}

// ---------------------------------------------------------------------------
// (qt-set-display handle) -> t

static lbm_value ext_qt_set_display(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;

  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmDisplayWidget *d = qobject_cast<QLbmDisplayWidget*>(s_widgets.value(handle, nullptr));
  if (!d) {
    lbm_set_error_reason("qt-set-display: invalid handle");
    return ENC_SYM_EERROR;
  }
  s_activeDisplay = d;
  lbm_display_extensions_set_callbacks(qt_render_image, nullptr, nullptr);
  return ENC_SYM_TRUE;
}

// ---------------------------------------------------------------------------
// (qt-set-button-label handle label) -> t

static lbm_value ext_qt_set_button_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;

  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmButtonWidget *btn = qobject_cast<QLbmButtonWidget *>(s_widgets.value(handle, nullptr));
  if (!btn) {
    lbm_set_error_reason("qt-set-button-label: invalid handle");
    return ENC_SYM_EERROR;
  }

  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString label = QString::fromUtf8((const char *)arr->data);

  QMetaObject::invokeMethod(btn, [btn, label]() {
    btn->setLabel(label);
  }, Qt::QueuedConnection);

  return ENC_SYM_TRUE;
}

// ---------------------------------------------------------------------------
// (qt-widget-add-display container-handle w h) -> handle

static lbm_value ext_qt_widget_add_display(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-display: invalid container handle");
    return ENC_SYM_EERROR;
  }

  int w = (int)lbm_dec_as_i32(args[1]);
  int h = (int)lbm_dec_as_i32(args[2]);
  if (w <= 0 || h <= 0) return ENC_SYM_TERROR;

  int handle = createDisplay(container, w, h, parseAttrs(args, argn, 3));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

// ---------------------------------------------------------------------------
// (qt-widget-add-button container-handle label) -> handle

static lbm_value ext_qt_widget_add_button(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-button: invalid container handle");
    return ENC_SYM_EERROR;
  }

  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString label = QString::fromUtf8((const char *)arr->data);

  int handle = createButton(container, label, parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

// ---------------------------------------------------------------------------
// (qt-widget-add-container container-handle layout-sym) -> handle

static lbm_value ext_qt_widget_add_container(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_symbol(args[1]))
    return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *parent = getContainer(ch);
  if (!parent) {
    lbm_set_error_reason("qt-widget-add-container: invalid container handle");
    return ENC_SYM_EERROR;
  }

  QLbmLayout layout = layoutFromSymbol(args[1]);
  int handle = createContainer(parent, layout, parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

// ---------------------------------------------------------------------------
// (qt-widget-add-stretch container-handle [attrs...]) -> handle
// (qt-widget-add-stretch container-handle 'horizontal [attrs...]) -> handle

static lbm_value ext_qt_widget_add_stretch(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-stretch: invalid container handle");
    return ENC_SYM_EERROR;
  }
  Qt::Orientation orientation = Qt::Vertical;
  lbm_uint attr_start = 1;
  if (argn >= 2 && lbm_is_symbol(args[1])) {
    const char *name = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
    if (name && strcmp(name, "horizontal") == 0) {
      orientation = Qt::Horizontal;
      attr_start = 2;
    }
  }
  int handle = createStretchInner(container, orientation, parseAttrs(args, argn, attr_start));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

// ---------------------------------------------------------------------------
// (qt-widget-set-max-width handle w) -> t
// (qt-widget-set-min-width handle w) -> t

static lbm_value ext_qt_widget_set_max_width(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-max-width: invalid handle");
    return ENC_SYM_EERROR;
  }
  int width = (int)lbm_dec_as_i32(args[1]);
  QMetaObject::invokeMethod(w, [w, width]() {
    w->setMaximumWidth(width);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_widget_set_min_width(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-min-width: invalid handle");
    return ENC_SYM_EERROR;
  }
  int width = (int)lbm_dec_as_i32(args[1]);
  QMetaObject::invokeMethod(w, [w, width]() {
    w->setMinimumWidth(width);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// ---------------------------------------------------------------------------
// Remove widget and all its LBM-managed descendants from the registry.
// Must be called before deleteLater() so parent relationships are still intact.

static void removeFromRegistry(QLbmWidget *w) {
  QList<int> toRemove;
  for (auto it = s_widgets.begin(); it != s_widgets.end(); ++it) {
    QLbmWidget *candidate = it.value();
    if (candidate == w || w->isAncestorOf(candidate))
      toRemove.append(it.key());
  }
  for (int h : toRemove)
    s_widgets.remove(h);
  if (s_activeDisplay && (s_activeDisplay == w ||
                          w->isAncestorOf(s_activeDisplay)))
    s_activeDisplay = nullptr;
}

// ---------------------------------------------------------------------------
// (qt-widget-remove handle) -> t

static lbm_value ext_qt_widget_remove(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;

  int handle = (int)lbm_dec_as_i32(args[0]);
  if (handle == s_rootHandle) {
    lbm_set_error_reason("qt-widget-remove: cannot remove root widget");
    return ENC_SYM_EERROR;
  }
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-remove: invalid handle");
    return ENC_SYM_EERROR;
  }
  QLbmContainerWidget *parent = qobject_cast<QLbmContainerWidget*>(w->parentWidget());
  if (!parent) {
    lbm_set_error_reason("qt-widget-remove: widget has no container parent");
    return ENC_SYM_EERROR;
  }

  removeFromRegistry(w);

  QMetaObject::invokeMethod(parent, [parent, w]() {
    parent->removeChildWidget(w);
  }, Qt::BlockingQueuedConnection);

  return ENC_SYM_TRUE;
}

// ---------------------------------------------------------------------------

void lbm_qt_extensions_init(void) {
  lbm_add_extension("qt-root",                  ext_qt_root);
  lbm_add_extension("qt-set-display",           ext_qt_set_display);
  lbm_add_extension("qt-set-button-label",      ext_qt_set_button_label);
  lbm_add_extension("qt-widget-add-display",    ext_qt_widget_add_display);
  lbm_add_extension("qt-widget-add-button",     ext_qt_widget_add_button);
  lbm_add_extension("qt-widget-add-container",  ext_qt_widget_add_container);
  lbm_add_extension("qt-widget-add-stretch",    ext_qt_widget_add_stretch);
  lbm_add_extension("qt-widget-set-max-width",  ext_qt_widget_set_max_width);
  lbm_add_extension("qt-widget-set-min-width",  ext_qt_widget_set_min_width);
  lbm_add_extension("qt-widget-remove",         ext_qt_widget_remove);
}
