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
#include "QLbmCheckboxWidget.h"
#include "QLbmRadioWidget.h"
#include "QLbmSpinboxIWidget.h"
#include "QLbmSpinboxFWidget.h"
#include "QLbmTextfieldWidget.h"
#include "QLbmLabelWidget.h"
#include "QLbmSliderWidget.h"
#include "QLbmComboWidget.h"
#include "QLbmPlotWidget.h"
#include "QLispBM.h"
#include "QLbmValue.h"

#include <QMetaObject>
#include <QImage>
#include <QHash>
#include <QButtonGroup>
#include <QRadioButton>
#include <QScrollArea>

extern "C" {
#include "lispbm.h"
#include "heap.h"
#include "extensions/display_extensions.h"
}

#include <cstring>

// ////////////////////////////////////////////////////////////
// Module state

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

// ////////////////////////////////////////////////////////////
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

// ////////////////////////////////////////////////////////////
// Widget attributes

struct QtWidgetAttrs {
  int     max_width  = -1;
  int     min_width  = -1;
  int     max_height = -1;
  int     min_height = -1;
  int     pos_x      = -1;  // grid column (-1 = auto)
  int     pos_y      = -1;  // grid row    (-1 = auto)
  int     visible    = -1;  // -1 = unset, 0 = hidden, 1 = visible
  int     enabled    = -1;  // -1 = unset, 0 = disabled, 1 = enabled
  QString style      = QString();
  bool    scrollV    = false;
  bool    scrollH    = false;
};

static QtWidgetAttrs parseAttrs(lbm_value *args, lbm_uint argn, lbm_uint start) {
  QtWidgetAttrs a;
  for (lbm_uint i = start; i < argn; i++) {
    lbm_value attr = args[i];
    const char *key = nullptr;
    lbm_value   val = ENC_SYM_NIL;

    if (lbm_is_symbol(attr)) {
      key = lbm_get_name_by_symbol(lbm_dec_sym(attr));
      if (key) {
        if (strcmp(key, "scroll")   == 0) { a.scrollV = true; a.scrollH = true; }
        else if (strcmp(key, "scroll-v") == 0) a.scrollV = true;
        else if (strcmp(key, "scroll-h") == 0) a.scrollH = true;
      }
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
    else if (strcmp(key, "pos-x") == 0 && lbm_is_cons(val))
      a.pos_x = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "pos-y") == 0 && lbm_is_cons(val))
      a.pos_y = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "visible") == 0 && lbm_is_cons(val))
      a.visible = lbm_is_symbol_nil(lbm_car(val)) ? 0 : 1;
    else if (strcmp(key, "enabled") == 0 && lbm_is_cons(val))
      a.enabled = lbm_is_symbol_nil(lbm_car(val)) ? 0 : 1;
    else if (strcmp(key, "style") == 0 && lbm_is_cons(val) &&
             lbm_is_array_r(lbm_car(val))) {
      lbm_array_header_t *arr = lbm_dec_array_r(lbm_car(val));
      if (arr) a.style = QString::fromUtf8((const char *)arr->data);
    }
  }
  return a;
}

static void addToContainer(QLbmContainerWidget *container, QLbmWidget *child,
                           const QtWidgetAttrs &attrs) {
  if (attrs.pos_x >= 0 && attrs.pos_y >= 0)
    container->addChildWidgetAt(child, attrs.pos_y, attrs.pos_x);
  else
    container->addChildWidget(child);
}

static void applyAttrs(QWidget *w, const QtWidgetAttrs &a) {
  if (a.max_width  >= 0) w->setMaximumWidth(a.max_width);
  if (a.min_width  >= 0) w->setMinimumWidth(a.min_width);
  if (a.max_height >= 0) w->setMaximumHeight(a.max_height);
  if (a.min_height >= 0) w->setMinimumHeight(a.min_height);
  if (a.visible   >= 0) w->setVisible(a.visible != 0);
  if (a.enabled   >= 0) w->setEnabled(a.enabled != 0);
  if (!a.style.isEmpty()) w->setStyleSheet(a.style);
}

// ////////////////////////////////////////////////////////////
// Image handling

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

// ////////////////////////////////////////////////////////////
// Widget creation helpers

static int createDisplay(QLbmContainerWidget *container, int w, int h,
                         const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, w, h, attrs, &handle]() {
    auto *display = new QLbmDisplayWidget(w, h, container);
    applyAttrs(display, attrs);
    handle = registerWidget(display);
    addToContainer(container, display, attrs);
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
    addToContainer(container, btn, attrs);
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
    auto *container = new QLbmContainerWidget(layout);
    handle = registerWidget(container);
    if (attrs.scrollV || attrs.scrollH) {
      // SetMinimumSize makes the container enforce its layout's minimum height
      // as a hard constraint, preventing the scroll area from squishing it.
      container->layout()->setSizeConstraint(QLayout::SetMinimumSize);
      auto *sa = new QScrollArea(parent);
      sa->setWidgetResizable(true);
      sa->setHorizontalScrollBarPolicy(attrs.scrollH ? Qt::ScrollBarAsNeeded
                                                     : Qt::ScrollBarAlwaysOff);
      sa->setVerticalScrollBarPolicy(attrs.scrollV ? Qt::ScrollBarAsNeeded
                                                   : Qt::ScrollBarAlwaysOff);
      sa->setWidget(container);
      applyAttrs(sa, attrs);  // size/visibility constrain the viewport, not the content
      parent->addExternalWidget(sa);
    } else {
      applyAttrs(container, attrs);
      addToContainer(parent, container, attrs);
    }
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
    addToContainer(container, stretch, attrs);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createCheckbox(QLbmContainerWidget *container, const QString &label,
                          const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, label, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *cb = new QLbmCheckboxWidget(label, handle, container);
    applyAttrs(cb, attrs);
    s_widgets.insert(handle, cb);
    addToContainer(container, cb, attrs);
    QObject::connect(cb, &QLbmCheckboxWidget::changed, [](int h, bool checked) {
      if (QLispBM::instance())
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("checkbox-changed"),
                               QLbmValue::fromI(h),
                               checked ? QLbmValue::fromSymbol("t") : QLbmValue()}));
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createRadio(QLbmContainerWidget *container, const QString &label,
                       const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, label, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *radio = new QLbmRadioWidget(label, handle, container);
    applyAttrs(radio, attrs);
    s_widgets.insert(handle, radio);
    container->radioGroup()->addButton(radio->radioButton());
    addToContainer(container, radio, attrs);
    QObject::connect(radio, &QLbmRadioWidget::selected, [](int h) {
      if (QLispBM::instance())
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("radio-changed"),
                               QLbmValue::fromI(h)}));
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createSpinboxI(QLbmContainerWidget *container, int min, int max,
                          const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, min, max, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *sb = new QLbmSpinboxIWidget(min, max, handle, container);
    applyAttrs(sb, attrs);
    s_widgets.insert(handle, sb);
    addToContainer(container, sb, attrs);
    QObject::connect(sb, &QLbmSpinboxIWidget::committed, [](int h, int val) {
      if (QLispBM::instance())
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("spinbox-changed"),
                               QLbmValue::fromI(h),
                               QLbmValue::fromI(val)}));
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createSpinboxF(QLbmContainerWidget *container, double min, double max, double step,
                          const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, min, max, step, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *sb = new QLbmSpinboxFWidget(min, max, step, handle, container);
    applyAttrs(sb, attrs);
    s_widgets.insert(handle, sb);
    addToContainer(container, sb, attrs);
    QObject::connect(sb, &QLbmSpinboxFWidget::committed, [](int h, double val) {
      if (QLispBM::instance())
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("spinbox-changed"),
                               QLbmValue::fromI(h),
                               QLbmValue::fromFloat((float)val)}));
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createTextfield(QLbmContainerWidget *container, const QString &placeholder,
                           const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, placeholder, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *tf = new QLbmTextfieldWidget(placeholder, handle, container);
    applyAttrs(tf, attrs);
    s_widgets.insert(handle, tf);
    addToContainer(container, tf, attrs);
    QObject::connect(tf, &QLbmTextfieldWidget::committed, [](int h, const QString &text) {
      if (QLispBM::instance()) {
        QByteArray ba = text.toUtf8();
        ba.append('\0');
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("textfield-commit"),
                               QLbmValue::fromI(h),
                               QLbmValue::fromByteArray(ba)}));
      }
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static lbm_value ext_qt_root(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (s_rootHandle < 0) {
    lbm_set_error_reason("qt-root: no root widget registered");
    return ENC_SYM_EERROR;
  }
  return lbm_enc_i(s_rootHandle);
}

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

static lbm_value ext_qt_widget_set_visible(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-visible: invalid handle");
    return ENC_SYM_EERROR;
  }
  bool v = !lbm_is_symbol_nil(args[1]);
  QMetaObject::invokeMethod(w, [w, v]() { w->setVisible(v); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_widget_get_visible(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-get-visible: invalid handle");
    return ENC_SYM_EERROR;
  }
  bool v = false;
  QMetaObject::invokeMethod(w, [w, &v]() { v = w->isVisible(); },
                            Qt::BlockingQueuedConnection);
  return v ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_qt_widget_set_enabled(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-enabled: invalid handle");
    return ENC_SYM_EERROR;
  }
  bool v = !lbm_is_symbol_nil(args[1]);
  QMetaObject::invokeMethod(w, [w, v]() { w->setEnabled(v); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_widget_get_enabled(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-get-enabled: invalid handle");
    return ENC_SYM_EERROR;
  }
  bool v = false;
  QMetaObject::invokeMethod(w, [w, &v]() { v = w->isEnabled(); },
                            Qt::BlockingQueuedConnection);
  return v ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_qt_widget_set_style(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-style: invalid handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString style = QString::fromUtf8((const char *)arr->data);
  QMetaObject::invokeMethod(w, [w, style]() { w->setStyleSheet(style); },
                            Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

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

static lbm_value ext_qt_widget_set_max_height(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-max-height: invalid handle");
    return ENC_SYM_EERROR;
  }
  int height = (int)lbm_dec_as_i32(args[1]);
  QMetaObject::invokeMethod(w, [w, height]() {
    w->setMaximumHeight(height);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_widget_set_min_height(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-min-height: invalid handle");
    return ENC_SYM_EERROR;
  }
  int height = (int)lbm_dec_as_i32(args[1]);
  QMetaObject::invokeMethod(w, [w, height]() {
    w->setMinimumHeight(height);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// ////////////////////////////////////////////////////////////
// Extensions

static int createLabel(QLbmContainerWidget *container, const QString &text,
                       const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, text, attrs, &handle]() {
    auto *label = new QLbmLabelWidget(text, container);
    applyAttrs(label, attrs);
    handle = registerWidget(label);
    addToContainer(container, label, attrs);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createSlider(QLbmContainerWidget *container, int min, int max,
                        Qt::Orientation orientation,
                        const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, min, max, orientation, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *slider = new QLbmSliderWidget(min, max, orientation, handle, container);
    applyAttrs(slider, attrs);
    s_widgets.insert(handle, slider);
    addToContainer(container, slider, attrs);
    QObject::connect(slider, &QLbmSliderWidget::changed, [](int h, int val) {
      if (QLispBM::instance())
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("slider-changed"),
                               QLbmValue::fromI(h),
                               QLbmValue::fromI(val)}));
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static QStringList lbmListToStringList(lbm_value list) {
  QStringList result;
  for (lbm_value c = list; lbm_is_cons(c); c = lbm_cdr(c)) {
    lbm_value item = lbm_car(c);
    if (lbm_is_array_r(item)) {
      lbm_array_header_t *arr = lbm_dec_array_r(item);
      if (arr) result.append(QString::fromUtf8((const char *)arr->data));
    }
  }
  return result;
}

static int createCombo(QLbmContainerWidget *container, const QStringList &items,
                       const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, items, attrs, &handle]() {
    handle = s_nextHandle++;
    auto *combo = new QLbmComboWidget(items, handle, container);
    applyAttrs(combo, attrs);
    s_widgets.insert(handle, combo);
    addToContainer(container, combo, attrs);
    QObject::connect(combo, &QLbmComboWidget::changed, [](int h, int idx) {
      if (QLispBM::instance())
        QLispBM::instance()->sendEvent(
          QLbmValue::fromList({QLbmValue::fromSymbol("combo-changed"),
                               QLbmValue::fromI(h),
                               QLbmValue::fromI(idx)}));
    });
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static lbm_value ext_qt_widget_add_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-label: invalid container handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString text = QString::fromUtf8((const char *)arr->data);
  int handle = createLabel(container, text, parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_widget_add_slider(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-slider: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int min = (int)lbm_dec_as_i32(args[1]);
  int max = (int)lbm_dec_as_i32(args[2]);
  Qt::Orientation orientation = Qt::Horizontal;
  lbm_uint attr_start = 3;
  if (argn >= 4 && lbm_is_symbol(args[3])) {
    const char *name = lbm_get_name_by_symbol(lbm_dec_sym(args[3]));
    if (name && strcmp(name, "vertical") == 0) {
      orientation = Qt::Vertical;
      attr_start = 4;
    }
  }
  int handle = createSlider(container, min, max, orientation,
                            parseAttrs(args, argn, attr_start));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_combo_get_item(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  int index  = (int)lbm_dec_as_i32(args[1]);
  QLbmComboWidget *cb = qobject_cast<QLbmComboWidget *>(s_widgets.value(handle, nullptr));
  if (!cb) {
    lbm_set_error_reason("qt-combo-get-item: invalid handle");
    return ENC_SYM_EERROR;
  }
  QString text;
  QMetaObject::invokeMethod(cb, [cb, index, &text]() {
    text = cb->getItem(index);
  }, Qt::BlockingQueuedConnection);
  QByteArray ba = text.toUtf8();
  lbm_value arr;
  if (!lbm_heap_allocate_array(&arr, (lbm_uint)(ba.size() + 1))) return ENC_SYM_MERROR;
  lbm_array_header_t *ahdr = lbm_dec_array_rw(arr);
  if (!ahdr) return ENC_SYM_MERROR;
  memcpy(ahdr->data, ba.constData(), (size_t)ba.size());
  ((uint8_t *)ahdr->data)[ba.size()] = 0;
  return arr;
}

static lbm_value ext_qt_widget_add_combo(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-combo: invalid container handle");
    return ENC_SYM_EERROR;
  }
  QStringList items = lbmListToStringList(args[1]);
  int handle = createCombo(container, items, parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

// removeFromRegistry must be called before deleteLater() while parent
// relationships are still intact.
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
  QLbmContainerWidget *parent = nullptr;
  for (QWidget *p = w->parentWidget(); p; p = p->parentWidget()) {
    if ((parent = qobject_cast<QLbmContainerWidget *>(p)))
      break;
  }
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

static lbm_value ext_qt_widget_add_checkbox(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-checkbox: invalid container handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString label = QString::fromUtf8((const char *)arr->data);

  int handle = createCheckbox(container, label, parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_widget_add_radio(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-radio: invalid container handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString label = QString::fromUtf8((const char *)arr->data);

  int handle = createRadio(container, label, parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_widget_add_spinbox_i(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-spinbox-i: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int min = (int)lbm_dec_as_i32(args[1]);
  int max = (int)lbm_dec_as_i32(args[2]);

  int handle = createSpinboxI(container, min, max, parseAttrs(args, argn, 3));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_widget_add_spinbox_f(lbm_value *args, lbm_uint argn) {
  if (argn < 4 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) ||
      !lbm_is_number(args[2]) || !lbm_is_number(args[3]))
    return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-spinbox-f: invalid container handle");
    return ENC_SYM_EERROR;
  }
  double min  = (double)lbm_dec_as_float(args[1]);
  double max  = (double)lbm_dec_as_float(args[2]);
  double step = (double)lbm_dec_as_float(args[3]);

  int handle = createSpinboxF(container, min, max, step, parseAttrs(args, argn, 4));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_widget_add_textfield(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;

  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-textfield: invalid container handle");
    return ENC_SYM_EERROR;
  }

  QString placeholder;
  lbm_uint attr_start = 1;
  if (argn >= 2 && lbm_is_array_r(args[1])) {
    lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
    if (arr) placeholder = QString::fromUtf8((const char *)arr->data);
    attr_start = 2;
  }

  int handle = createTextfield(container, placeholder, parseAttrs(args, argn, attr_start));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_widget_get_value(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;

  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-get-value: invalid handle");
    return ENC_SYM_EERROR;
  }

  if (auto *cb = qobject_cast<QLbmCheckboxWidget *>(w)) {
    bool v = false;
    QMetaObject::invokeMethod(cb, [cb, &v]() { v = cb->getValue(); },
                              Qt::BlockingQueuedConnection);
    return v ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  if (auto *rb = qobject_cast<QLbmRadioWidget *>(w)) {
    bool v = false;
    QMetaObject::invokeMethod(rb, [rb, &v]() { v = rb->getValue(); },
                              Qt::BlockingQueuedConnection);
    return v ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  if (auto *sb = qobject_cast<QLbmSpinboxIWidget *>(w)) {
    int v = 0;
    QMetaObject::invokeMethod(sb, [sb, &v]() { v = sb->getValue(); },
                              Qt::BlockingQueuedConnection);
    return lbm_enc_i(v);
  }
  if (auto *sb = qobject_cast<QLbmSpinboxFWidget *>(w)) {
    double v = 0.0;
    QMetaObject::invokeMethod(sb, [sb, &v]() { v = sb->getValue(); },
                              Qt::BlockingQueuedConnection);
    return lbm_enc_float((float)v);
  }
  if (auto *tf = qobject_cast<QLbmTextfieldWidget *>(w)) {
    QString v;
    QMetaObject::invokeMethod(tf, [tf, &v]() { v = tf->getValue(); },
                              Qt::BlockingQueuedConnection);
    QByteArray ba = v.toUtf8();
    lbm_value arr;
    if (!lbm_heap_allocate_array(&arr, (lbm_uint)(ba.size() + 1))) return ENC_SYM_MERROR;
    lbm_array_header_t *ahdr = lbm_dec_array_rw(arr);
    if (!ahdr) return ENC_SYM_MERROR;
    memcpy(ahdr->data, ba.constData(), (size_t)ba.size());
    ((uint8_t *)ahdr->data)[ba.size()] = 0;
    return arr;
  }

  if (auto *lbl = qobject_cast<QLbmLabelWidget *>(w)) {
    QString v;
    QMetaObject::invokeMethod(lbl, [lbl, &v]() { v = lbl->getText(); },
                              Qt::BlockingQueuedConnection);
    QByteArray ba = v.toUtf8();
    lbm_value arr;
    if (!lbm_heap_allocate_array(&arr, (lbm_uint)(ba.size() + 1))) return ENC_SYM_MERROR;
    lbm_array_header_t *ahdr = lbm_dec_array_rw(arr);
    if (!ahdr) return ENC_SYM_MERROR;
    memcpy(ahdr->data, ba.constData(), (size_t)ba.size());
    ((uint8_t *)ahdr->data)[ba.size()] = 0;
    return arr;
  }
  if (auto *sl = qobject_cast<QLbmSliderWidget *>(w)) {
    int v = 0;
    QMetaObject::invokeMethod(sl, [sl, &v]() { v = sl->getValue(); },
                              Qt::BlockingQueuedConnection);
    return lbm_enc_i(v);
  }
  if (auto *cb = qobject_cast<QLbmComboWidget *>(w)) {
    int v = 0;
    QMetaObject::invokeMethod(cb, [cb, &v]() { v = cb->getValue(); },
                              Qt::BlockingQueuedConnection);
    return lbm_enc_i(v);
  }
  lbm_set_error_reason("qt-widget-get-value: widget type has no gettable value");
  return ENC_SYM_EERROR;
}

static lbm_value ext_qt_widget_set_value(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;

  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmWidget *w = s_widgets.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-widget-set-value: invalid handle");
    return ENC_SYM_EERROR;
  }

  if (auto *cb = qobject_cast<QLbmCheckboxWidget *>(w)) {
    bool v = !lbm_is_symbol_nil(args[1]);
    QMetaObject::invokeMethod(cb, [cb, v]() { cb->setValue(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (auto *rb = qobject_cast<QLbmRadioWidget *>(w)) {
    bool v = !lbm_is_symbol_nil(args[1]);
    QMetaObject::invokeMethod(rb, [rb, v]() { rb->setValue(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (auto *sb = qobject_cast<QLbmSpinboxIWidget *>(w)) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    int v = (int)lbm_dec_as_i32(args[1]);
    QMetaObject::invokeMethod(sb, [sb, v]() { sb->setValue(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (auto *sb = qobject_cast<QLbmSpinboxFWidget *>(w)) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    double v = (double)lbm_dec_as_float(args[1]);
    QMetaObject::invokeMethod(sb, [sb, v]() { sb->setValue(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (auto *tf = qobject_cast<QLbmTextfieldWidget *>(w)) {
    if (!lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
    lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
    if (!arr) return ENC_SYM_TERROR;
    QString v = QString::fromUtf8((const char *)arr->data);
    QMetaObject::invokeMethod(tf, [tf, v]() { tf->setValue(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }

  if (auto *lbl = qobject_cast<QLbmLabelWidget *>(w)) {
    if (!lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
    lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
    if (!arr) return ENC_SYM_TERROR;
    QString v = QString::fromUtf8((const char *)arr->data);
    QMetaObject::invokeMethod(lbl, [lbl, v]() { lbl->setText(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (auto *sl = qobject_cast<QLbmSliderWidget *>(w)) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    int v = (int)lbm_dec_as_i32(args[1]);
    QMetaObject::invokeMethod(sl, [sl, v]() { sl->setValue(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (auto *cb = qobject_cast<QLbmComboWidget *>(w)) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    int v = (int)lbm_dec_as_i32(args[1]);
    QMetaObject::invokeMethod(cb, [cb, v]() { cb->setValue(v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  lbm_set_error_reason("qt-widget-set-value: widget type has no settable value");
  return ENC_SYM_EERROR;
}

// ////////////////////////////////////////////////////////////
// Plot extensions

static QLbmPlotWidget *getPlot(int handle) {
  return qobject_cast<QLbmPlotWidget *>(s_widgets.value(handle, nullptr));
}

static int createPlot(QLbmContainerWidget *container,
                      const QtWidgetAttrs &attrs = QtWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, attrs, &handle]() {
    auto *plot = new QLbmPlotWidget(container);
    applyAttrs(plot, attrs);
    handle = registerWidget(plot);
    addToContainer(container, plot, attrs);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static QVector<double> lbmListToDoubleVec(lbm_value list) {
  QVector<double> v;
  for (lbm_value c = list; lbm_is_cons(c); c = lbm_cdr(c))
    v.append((double)lbm_dec_as_float(lbm_car(c)));
  return v;
}

static lbm_value ext_qt_widget_add_plot(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int ch = (int)lbm_dec_as_i32(args[0]);
  QLbmContainerWidget *container = getContainer(ch);
  if (!container) {
    lbm_set_error_reason("qt-widget-add-plot: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createPlot(container, parseAttrs(args, argn, 1));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_plot_add_graph(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-add-graph: invalid handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString name = QString::fromUtf8((const char *)arr->data);
  int graphId = -1;
  QMetaObject::invokeMethod(plot, [plot, name, &graphId]() {
    graphId = plot->addGraph(name);
  }, Qt::BlockingQueuedConnection);
  return graphId >= 0 ? lbm_enc_i(graphId) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_plot_set_data(lbm_value *args, lbm_uint argn) {
  if (argn < 4 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle  = (int)lbm_dec_as_i32(args[0]);
  int graphId = (int)lbm_dec_as_i32(args[1]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-data: invalid handle");
    return ENC_SYM_EERROR;
  }
  QVector<double> xs = lbmListToDoubleVec(args[2]);
  QVector<double> ys = lbmListToDoubleVec(args[3]);
  QMetaObject::invokeMethod(plot, [plot, graphId, xs, ys]() {
    plot->setData(graphId, xs, ys);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_add_point(lbm_value *args, lbm_uint argn) {
  if (argn < 4 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) ||
      !lbm_is_number(args[2]) || !lbm_is_number(args[3]))
    return ENC_SYM_TERROR;
  int handle  = (int)lbm_dec_as_i32(args[0]);
  int graphId = (int)lbm_dec_as_i32(args[1]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-add-point: invalid handle");
    return ENC_SYM_EERROR;
  }
  double x = (double)lbm_dec_as_float(args[2]);
  double y = (double)lbm_dec_as_float(args[3]);
  QMetaObject::invokeMethod(plot, [plot, graphId, x, y]() {
    plot->addPoint(graphId, x, y);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_clear_graph(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle  = (int)lbm_dec_as_i32(args[0]);
  int graphId = (int)lbm_dec_as_i32(args[1]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-clear-graph: invalid handle");
    return ENC_SYM_EERROR;
  }
  QMetaObject::invokeMethod(plot, [plot, graphId]() {
    plot->clearGraph(graphId);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_clear(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-clear: invalid handle");
    return ENC_SYM_EERROR;
  }
  QMetaObject::invokeMethod(plot, [plot]() {
    plot->clearAll();
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_rescale(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-rescale: invalid handle");
    return ENC_SYM_EERROR;
  }
  QMetaObject::invokeMethod(plot, [plot]() {
    plot->rescale();
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_replot(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-replot: invalid handle");
    return ENC_SYM_EERROR;
  }
  QMetaObject::invokeMethod(plot, [plot]() {
    plot->replot();
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_set_x_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-x-label: invalid handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString label = QString::fromUtf8((const char *)arr->data);
  QMetaObject::invokeMethod(plot, [plot, label]() {
    plot->setXLabel(label);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_set_y_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-y-label: invalid handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString label = QString::fromUtf8((const char *)arr->data);
  QMetaObject::invokeMethod(plot, [plot, label]() {
    plot->setYLabel(label);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_set_x_log(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-x-log: invalid handle");
    return ENC_SYM_EERROR;
  }
  bool log = !lbm_is_symbol_nil(args[1]);
  QMetaObject::invokeMethod(plot, [plot, log]() {
    plot->setXLog(log);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_set_y_log(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-y-log: invalid handle");
    return ENC_SYM_EERROR;
  }
  bool log = !lbm_is_symbol_nil(args[1]);
  QMetaObject::invokeMethod(plot, [plot, log]() {
    plot->setYLog(log);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_set_x_range(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-x-range: invalid handle");
    return ENC_SYM_EERROR;
  }
  double lower = (double)lbm_dec_as_float(args[1]);
  double upper = (double)lbm_dec_as_float(args[2]);
  QMetaObject::invokeMethod(plot, [plot, lower, upper]() {
    plot->setXRange(lower, upper);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_set_y_range(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-y-range: invalid handle");
    return ENC_SYM_EERROR;
  }
  double lower = (double)lbm_dec_as_float(args[1]);
  double upper = (double)lbm_dec_as_float(args[2]);
  QMetaObject::invokeMethod(plot, [plot, lower, upper]() {
    plot->setYRange(lower, upper);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_plot_save_image(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-save-image: invalid handle");
    return ENC_SYM_EERROR;
  }
  lbm_array_header_t *arr = lbm_dec_array_r(args[1]);
  if (!arr) return ENC_SYM_TERROR;
  QString path = QString::fromUtf8((const char *)arr->data);
  bool ok = false;
  QMetaObject::invokeMethod(plot, [plot, path, &ok]() {
    ok = plot->saveImage(path);
  }, Qt::BlockingQueuedConnection);
  return ok ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_qt_plot_set_max_points(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) ||
      !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  int handle  = (int)lbm_dec_as_i32(args[0]);
  int graphId = (int)lbm_dec_as_i32(args[1]);
  int maxPts  = (int)lbm_dec_as_i32(args[2]);
  QLbmPlotWidget *plot = getPlot(handle);
  if (!plot) {
    lbm_set_error_reason("qt-plot-set-max-points: invalid handle");
    return ENC_SYM_EERROR;
  }
  QMetaObject::invokeMethod(plot, [plot, graphId, maxPts]() {
    plot->setMaxPoints(graphId, maxPts);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// ////////////////////////////////////////////////////////////
// Extension registration

void lbm_qt_extensions_init(void) {
  lbm_add_extension("qt-root",                  ext_qt_root);
  lbm_add_extension("qt-set-display",           ext_qt_set_display);
  lbm_add_extension("qt-set-button-label",      ext_qt_set_button_label);
  lbm_add_extension("qt-widget-add-display",    ext_qt_widget_add_display);
  lbm_add_extension("qt-widget-add-button",     ext_qt_widget_add_button);
  lbm_add_extension("qt-widget-add-container",  ext_qt_widget_add_container);
  lbm_add_extension("qt-widget-add-stretch",    ext_qt_widget_add_stretch);
  lbm_add_extension("qt-widget-set-visible",    ext_qt_widget_set_visible);
  lbm_add_extension("qt-widget-get-visible",    ext_qt_widget_get_visible);
  lbm_add_extension("qt-widget-set-enabled",    ext_qt_widget_set_enabled);
  lbm_add_extension("qt-widget-get-enabled",    ext_qt_widget_get_enabled);
  lbm_add_extension("qt-widget-set-style",      ext_qt_widget_set_style);
  lbm_add_extension("qt-widget-set-max-width",  ext_qt_widget_set_max_width);
  lbm_add_extension("qt-widget-set-min-width",  ext_qt_widget_set_min_width);
  lbm_add_extension("qt-widget-set-max-height", ext_qt_widget_set_max_height);
  lbm_add_extension("qt-widget-set-min-height", ext_qt_widget_set_min_height);
  lbm_add_extension("qt-widget-remove",         ext_qt_widget_remove);
  lbm_add_extension("qt-widget-add-checkbox",   ext_qt_widget_add_checkbox);
  lbm_add_extension("qt-widget-add-radio",      ext_qt_widget_add_radio);
  lbm_add_extension("qt-widget-add-spinbox-i",  ext_qt_widget_add_spinbox_i);
  lbm_add_extension("qt-widget-add-spinbox-f",  ext_qt_widget_add_spinbox_f);
  lbm_add_extension("qt-widget-add-textfield",  ext_qt_widget_add_textfield);
  lbm_add_extension("qt-widget-add-label",      ext_qt_widget_add_label);
  lbm_add_extension("qt-widget-add-slider",     ext_qt_widget_add_slider);
  lbm_add_extension("qt-widget-add-combo",      ext_qt_widget_add_combo);
  lbm_add_extension("qt-combo-get-item",        ext_qt_combo_get_item);
  lbm_add_extension("qt-widget-get-value",      ext_qt_widget_get_value);
  lbm_add_extension("qt-widget-set-value",      ext_qt_widget_set_value);
  lbm_add_extension("qt-widget-add-plot",       ext_qt_widget_add_plot);
  lbm_add_extension("qt-plot-add-graph",        ext_qt_plot_add_graph);
  lbm_add_extension("qt-plot-set-data",         ext_qt_plot_set_data);
  lbm_add_extension("qt-plot-add-point",        ext_qt_plot_add_point);
  lbm_add_extension("qt-plot-clear-graph",      ext_qt_plot_clear_graph);
  lbm_add_extension("qt-plot-clear",            ext_qt_plot_clear);
  lbm_add_extension("qt-plot-rescale",          ext_qt_plot_rescale);
  lbm_add_extension("qt-plot-replot",           ext_qt_plot_replot);
  lbm_add_extension("qt-plot-set-x-label",      ext_qt_plot_set_x_label);
  lbm_add_extension("qt-plot-set-y-label",      ext_qt_plot_set_y_label);
  lbm_add_extension("qt-plot-set-max-points",   ext_qt_plot_set_max_points);
  lbm_add_extension("qt-plot-set-x-log",        ext_qt_plot_set_x_log);
  lbm_add_extension("qt-plot-set-y-log",        ext_qt_plot_set_y_log);
  lbm_add_extension("qt-plot-set-x-range",      ext_qt_plot_set_x_range);
  lbm_add_extension("qt-plot-set-y-range",      ext_qt_plot_set_y_range);
  lbm_add_extension("qt-plot-save-image",       ext_qt_plot_save_image);
}
